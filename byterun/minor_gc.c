/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#include <string.h>
#include "config.h"
#include "fail.h"
#include "finalise.h"
#include "gc.h"
#include "gc_ctrl.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "signals.h"
#include "weak.h"

asize_t caml_minor_heap_size;
static void *caml_young_base = NULL;
CAMLexport char *caml_young_start = NULL, *caml_young_end = NULL;
CAMLexport char *caml_young_ptr = NULL, *caml_young_limit = NULL;

CAMLexport struct caml_ref_table
  caml_ref_table = { NULL, NULL, NULL, NULL, NULL, 0, 0},
  caml_weak_ref_table = { NULL, NULL, NULL, NULL, NULL, 0, 0};

int caml_in_minor_collection = 0;

#ifdef DEBUG
static unsigned long minor_gc_counter = 0;
#endif

void caml_alloc_table (struct caml_ref_table *tbl, asize_t sz, asize_t rsv)
{
  value **new_table;

  tbl->size = sz;
  tbl->reserve = rsv;
  new_table = (value **) caml_stat_alloc ((tbl->size + tbl->reserve)
                                          * sizeof (value *));
  if (tbl->base != NULL) caml_stat_free (tbl->base);
  tbl->base = new_table;
  tbl->ptr = tbl->base;
  tbl->threshold = tbl->base + tbl->size;
  tbl->limit = tbl->threshold;
  tbl->end = tbl->base + tbl->size + tbl->reserve;
}

static void reset_table (struct caml_ref_table *tbl)
{
  tbl->size = 0;
  tbl->reserve = 0;
  if (tbl->base != NULL) caml_stat_free (tbl->base);
  tbl->base = tbl->ptr = tbl->threshold = tbl->limit = tbl->end = NULL;
}

static void clear_table (struct caml_ref_table *tbl)
{
    tbl->ptr = tbl->base;
    tbl->limit = tbl->threshold;
}

void caml_set_minor_heap_size (asize_t size)
{
  char *new_heap;
  void *new_heap_base;

  Assert (size >= Minor_heap_min);
  Assert (size <= Minor_heap_max);
  Assert (size % sizeof (value) == 0);
  if (caml_young_ptr != caml_young_end) caml_minor_collection ();
                                    Assert (caml_young_ptr == caml_young_end);
  new_heap = caml_aligned_malloc(size, 0, &new_heap_base);
  if (new_heap == NULL) caml_raise_out_of_memory();
  if (caml_page_table_add(In_young, new_heap, new_heap + size) != 0)
    caml_raise_out_of_memory();

  if (caml_young_start != NULL){
    caml_page_table_remove(In_young, caml_young_start, caml_young_end);
    free (caml_young_base);
  }
  caml_young_base = new_heap_base;
  caml_young_start = new_heap;
  caml_young_end = new_heap + size;
  caml_young_limit = caml_young_start;
  caml_young_ptr = caml_young_end;
  caml_minor_heap_size = size;

  reset_table (&caml_ref_table);
  reset_table (&caml_weak_ref_table);
}

static value oldify_todo_list = 0;

static void oldify_one (value v, value *p)
{
  value result, field0;
  header_t hd;
  mlsize_t sz, i, infix_offset = 0;
  tag_t tag;

 tail_call:

  if (!Is_block (v) || !Is_young(v)) { 
    *p = v;
    return;
  }

  Assert (Hp_val (v) >= caml_young_ptr);
  hd = Hd_val (v);
  if (hd == 0) {         /* If already forwarded */
    *p = Field (v, 0);   /*  then forward pointer is first field. */
    return;
  }

  tag = Tag_hd (hd);
  if (tag == Infix_tag) {
    /* we need to move the entire closure, not just the part v points to */
    infix_offset = Infix_offset_hd (hd);
    v -= infix_offset;
    
    hd = Hd_val (v);
    if (hd == 0) {
      *p = Field(v, 0) + infix_offset; /* already forwarded */
      return;
    }
    tag = Tag_hd (hd);
    Assert (tag == Closure_tag);
  } else if (tag == Forward_tag) {
    /* we may want to short-circuit Forward_tag objects instead of copying */
    value f = Forward_val (v);
    if (Is_long(f)) {
      /* always short-circuit integers */
      *p = f;
      return;
    } else if (Is_young(f) || Is_in_value_area(f)) {
      tag_t ft = Tag_val (Hd_val (f) == 0 ? Field (f, 0) : f);
      if (ft != Forward_tag && ft != Lazy_tag && ft != Double_tag) {
        /* short-circuit all objects other than these three tags */
        v = f;
        goto tail_call;
      }
    }
  }

  sz = Wosize_hd (hd);
  result = caml_alloc_shr (sz, tag);
  *p = result + infix_offset;
  field0 = Field (v, 0);
  Hd_val (v) = 0;            /* Set forward flag */
  Field (v, 0) = result;     /*  and forward pointer. */

  if (tag < No_scan_tag){
    if (sz > 1){
      Field (result, 0) = field0;
      Field (result, 1) = oldify_todo_list;    /* Add this block */
      oldify_todo_list = v;                    /*  to the "to do" list. */
    }else{
      Assert (sz == 1);
      p = &Field (result, 0);
      v = field0;
      goto tail_call;
    }
  }else{
    Field (result, 0) = field0;
    for (i = 1; i < sz; i++) Field (result, i) = Field (v, i);
  }  
}

/* Make sure the minor heap is empty by performing a minor collection
   if needed.
*/
void caml_empty_minor_heap (void)
{
  value **r;

  if (caml_young_ptr != caml_young_end){
    caml_in_minor_collection = 1;
    caml_gc_message (0x02, "<", 0);
    caml_do_young_roots(&oldify_one);
    for (r = caml_ref_table.base; r < caml_ref_table.ptr; r++){
      oldify_one (**r, *r);
    }
    while (oldify_todo_list != 0){
      value v, new_v;
      mlsize_t i;
      /* we have to be careful to remove the first entry from the list
         before oldifying its fields. */
      v = oldify_todo_list;                /* Get the head. */
      Assert (Hd_val (v) == 0);            /* It must be forwarded. */
      new_v = Field (v, 0);                /* Follow forward pointer. */
      oldify_todo_list = Field (new_v, 1); /* Remove from list. */
      
      oldify_one(Field(new_v, 0), &Field(new_v, 0));
      for (i = 1; i < Wosize_val (new_v); i++){
        oldify_one(Field(v, i), &Field(new_v, i));
      }
    }

    for (r = caml_weak_ref_table.base; r < caml_weak_ref_table.ptr; r++){
      if (Is_block (**r) && Is_young (**r)){
        if (Hd_val (**r) == 0){
          **r = Field (**r, 0);
        }else{
          **r = caml_weak_none;
        }
      }
    }
    if (caml_young_ptr < caml_young_start) caml_young_ptr = caml_young_start;
    caml_stat_minor_words += Wsize_bsize (caml_young_end - caml_young_ptr);
    caml_young_ptr = caml_young_end;
    caml_young_limit = caml_young_start;
    clear_table (&caml_ref_table);
    clear_table (&caml_weak_ref_table);
    caml_gc_message (0x02, ">", 0);
    caml_in_minor_collection = 0;
  }
  caml_final_empty_young ();
#ifdef DEBUG
  {
    value *p;
    for (p = (value *) caml_young_start; p < (value *) caml_young_end; ++p){
      *p = Debug_free_minor;
    }
    ++ minor_gc_counter;
  }
#endif
}

/* Do a minor collection and a slice of major collection, call finalisation
   functions, etc.
   Leave the minor heap empty.
*/
CAMLexport void caml_minor_collection (void)
{
  intnat prev_alloc_words = caml_allocated_words;

  caml_empty_minor_heap ();

  caml_stat_promoted_words += caml_allocated_words - prev_alloc_words;
  ++ caml_stat_minor_collections;
  caml_major_collection_slice (0);
  caml_force_major_slice = 0;

  caml_final_do_calls ();

  caml_empty_minor_heap ();
}

CAMLexport value caml_check_urgent_gc (value extra_root)
{
  CAMLparam1 (extra_root);
  if (caml_force_major_slice) caml_minor_collection();
  CAMLreturn (extra_root);
}

void caml_realloc_ref_table (struct caml_ref_table *tbl)
{                                           Assert (tbl->ptr == tbl->limit);
                                            Assert (tbl->limit <= tbl->end);
                                      Assert (tbl->limit >= tbl->threshold);

  if (tbl->base == NULL){
    caml_alloc_table (tbl, caml_minor_heap_size / sizeof (value) / 8, 256);
  }else if (tbl->limit == tbl->threshold){
    caml_gc_message (0x08, "ref_table threshold crossed\n", 0);
    tbl->limit = tbl->end;
    caml_urge_major_slice ();
  }else{ /* This will almost never happen with the bytecode interpreter. */
    asize_t sz;
    asize_t cur_ptr = tbl->ptr - tbl->base;
                                             Assert (caml_force_major_slice);

    tbl->size *= 2;
    sz = (tbl->size + tbl->reserve) * sizeof (value *);
    caml_gc_message (0x08, "Growing ref_table to %"
                           ARCH_INTNAT_PRINTF_FORMAT "dk bytes\n",
                     (intnat) sz/1024);
    tbl->base = (value **) realloc ((char *) tbl->base, sz);
    if (tbl->base == NULL){
      caml_fatal_error ("Fatal error: ref_table overflow\n");
    }
    tbl->end = tbl->base + tbl->size + tbl->reserve;
    tbl->threshold = tbl->base + tbl->size;
    tbl->ptr = tbl->base + cur_ptr;
    tbl->limit = tbl->end;
  }
}
