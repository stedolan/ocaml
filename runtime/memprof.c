/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Jacques-Henri Joudan, projet Gallium, INRIA Paris          */
/*                                                                        */
/*   Copyright 2016 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <math.h>
#include <string.h>
#include "caml/memprof.h"
#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/signals.h"
#include "caml/memory.h"
#include "caml/minor_gc.h"
#include "caml/backtrace_prim.h"
#include "caml/weak.h"
#include "caml/stack.h"

static uint32_t mt_state[624];
static uint32_t mt_index;

/* [lambda] is the mean number of samples for each allocated word (including
   block headers). */
static double lambda = 0, lambda_rec = INFINITY;
static double next_sample_shr;
int caml_memprof_suspended = 0;
static intnat callstack_size = 0;
static value memprof_callback = Val_unit;

/* Whether memprof has been initialized.  */
static int init = 0;

/**** Statistical sampling ****/

static double mt_generate_uniform(void) {
  int i;
  uint32_t y;

  /* Mersenne twister PRNG */
  if (mt_index == 624) {
    for(i = 0; i < 227; i++) {
      y = (mt_state[i] & 0x80000000) + (mt_state[i+1] & 0x7fffffff);
      mt_state[i] = mt_state[i+397] ^ (y >> 1) ^ ((-(y&1)) & 0x9908b0df);
    }
    for(i = 227; i < 623; i++) {
      y = (mt_state[i] & 0x80000000) + (mt_state[i+1] & 0x7fffffff);
      mt_state[i] = mt_state[i-227] ^ (y >> 1) ^ ((-(y&1)) & 0x9908b0df);
    }
    y = (mt_state[623] & 0x80000000) + (mt_state[0] & 0x7fffffff);
    mt_state[623] = mt_state[396] ^ (y >> 1) ^ ((-(y&1)) & 0x9908b0df);
    mt_index = 0;
  }

  y = mt_state[mt_index];
  y = y ^ (y >> 11);
  y = y ^ ((y << 7) & 0x9d2c5680);
  y = y ^ ((y << 15) & 0xefc60000);
  y = y ^ (y >> 18);

  mt_index++;
  return y*2.3283064365386962890625e-10 + /* 2^-32 */
          1.16415321826934814453125e-10; /* 2^-33 */
}

static double mt_generate_exponential() {
  return -logf(mt_generate_uniform()) * lambda_rec;
}



/**** Interface with the OCaml code. ****/

CAMLprim value caml_memprof_set(value v) {
  CAMLparam1(v);
  double l = Double_val(Field(v, 0));
  intnat sz = Long_val(Field(v, 1));

  if(sz < 0 || !(l >= 0.) || l > 1.)
    caml_failwith("caml_memprof_set");

  if(!init) {
    int i;
    init = 1;

    mt_index = 624;
    mt_state[0] = 42;
    for(i = 1; i < 624; i++)
      mt_state[i] = 0x6c078965 * (mt_state[i-1] ^ (mt_state[i-1] >> 30)) + i;

    caml_register_generational_global_root(&memprof_callback);
  }

  lambda = l;
  lambda_rec = l == 0 ? INFINITY : 1/l;
  next_sample_shr = mt_generate_exponential();
  callstack_size = sz;
  caml_modify_generational_global_root(&memprof_callback, Field(v, 2));

  CAMLreturn(Val_unit);
}

/* Cf. Gc.Memprof.alloc_kind */
enum ml_alloc_kind {
  Minor = Val_long(0),
  Major = Val_long(1),
  Serialized = Val_long(2)
};

static value do_callback(tag_t tag, intnat wosize, int32_t occurences,
                         value callstack, enum ml_alloc_kind cb_kind) {
  CAMLparam1(callstack);
  CAMLlocal1(sample_info);
  CAMLassert(occurences > 0);

  sample_info = caml_alloc_small(5, 0);
  Field(sample_info, 0) = Val_long(occurences);
  Field(sample_info, 1) = cb_kind;
  Field(sample_info, 2) = Val_long(tag);
  Field(sample_info, 3) = Val_long(wosize);
  Field(sample_info, 4) = callstack;

  CAMLreturn(caml_callback_exn(memprof_callback, sample_info));
}

void caml_memprof_set_suspended(int new_suspended) {
  caml_memprof_suspended = new_suspended;
}

/**** Sampling procedures ****/

static value capture_callstack(int avoid_gc) {
  return caml_get_current_callstack_impl(callstack_size, avoid_gc);
}

struct caml_memprof_postponed_block {
  value block;
  value callstack;
  int32_t occurences;
  struct caml_memprof_postponed_block* next;
} static *caml_memprof_postponed_head = NULL;

/* When allocating in the major heap, we cannot call the callback, because
   [caml_alloc_shr] is guaranteed not to call the GC. Hence, this function
   determines if the block need to be sampled, and if so, it registers the
   block in the todo-list so that the callback call is performed when
   possible. */
void caml_memprof_track_alloc_shr(value block) {
  CAMLassert(Is_in_heap(block));
  int32_t occurences = 0;
  if (!caml_memprof_suspended && lambda > 0) {
    next_sample_shr -= Whsize_val(block);
    while (next_sample_shr <= 0) {
      occurences++;
      next_sample_shr += mt_generate_exponential();
    }
  }
  if(occurences > 0) {
    struct caml_memprof_postponed_block* pb =
      caml_stat_alloc_noexc(sizeof(struct caml_memprof_postponed_block));
    if(pb == NULL) return;
    pb->block = block;
    caml_register_generational_global_root(&pb->block);
    caml_memprof_set_suspended(1);
    pb->callstack = capture_callstack(1);
    caml_memprof_set_suspended(0);
    caml_register_generational_global_root(&pb->callstack);
    pb->occurences = occurences;
    pb->next = caml_memprof_postponed_head;
    caml_memprof_postponed_head = pb;
#ifndef NATIVE_CODE
    caml_something_to_do = 1;
#else
    caml_young_limit = caml_young_alloc_end;
#endif
  }
}

void caml_memprof_handle_postponed() {
  struct caml_memprof_postponed_block *p, *q;
  value ephe;

  if(caml_memprof_postponed_head == NULL)
    return;

  // We first reverse the list
  p = caml_memprof_postponed_head;
  q = caml_memprof_postponed_head->next;
  p->next = NULL;
  while(q != NULL) {
    struct caml_memprof_postponed_block* next = q->next;
    q->next = p;
    p = q;
    q = next;
  }
  caml_memprof_postponed_head = NULL;

#define NEXT_P \
  { struct caml_memprof_postponed_block* next = p->next;   \
    caml_remove_generational_global_root(&p->callstack);   \
    caml_remove_generational_global_root(&p->block);       \
    caml_stat_free(p);                                     \
    p = next; }

  caml_memprof_set_suspended(1);
  // We then do the actual iteration on postponed blocks
  while(p != NULL) {
    ephe = do_callback(Tag_val(p->block), Wosize_val(p->block),
                       p->occurences, p->callstack, Major);
    if (Is_exception_result(ephe)) {
      caml_memprof_set_suspended(0);
      // In the case of an exception, we just forget the entire list.
      while(p != NULL) NEXT_P;
      caml_raise(Extract_exception(ephe));
    }
    if(Is_block(ephe))
      caml_ephemeron_set_key(Field(ephe, 0), 0, p->block);
    NEXT_P;
  }
  caml_memprof_set_suspended(0);
}
