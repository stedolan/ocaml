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
#include "caml/misc.h"
#include "caml/compact.h"

static uint32_t mt_state[624];
static uint32_t mt_index;

/* [lambda] is the mean number of samples for each allocated word (including
   block headers). */
static double lambda = 0;
 /* Precomputed value of [1/log(1-lambda)], for fast sampling of
    geometric distribution.
    Dummy if [lambda = 0]. */
static double one_log1m_lambda;

int caml_memprof_suspended = 0;
static intnat callstack_size = 0;

static value callback_alloc_minor = Val_unit, callback_alloc_major = Val_unit,
             callback_promote = Val_unit, callback_dealloc_minor = Val_unit,
             callback_dealloc_major = Val_unit;

/* Pointer to the word following the next sample in the minor
   heap. Equals [Caml_state->young_alloc_start] if no sampling is planned in
   the current minor heap.
   Invariant: [caml_memprof_young_trigger <= Caml_state->young_ptr].
 */
value* caml_memprof_young_trigger;

/* Whether memprof has been initialized.  */
static int init = 0;

/**** Statistical sampling ****/

static double mt_generate_uniform(void)
{
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

/* Simulate a geometric variable of parameter [lambda].
   The result is clipped in [1..Max_long]
   Requires [lambda > 0]. */
static uintnat mt_generate_geom()
{
  /* We use the float versions of exp/log, since these functions are
     significantly faster, and we really don't need much precision
     here. The entropy contained in [next_mt_generate_geom] is anyway
     bounded by the entropy provided by [mt_generate_uniform], which
     is 32bits. */
  double res = 1 + logf(mt_generate_uniform()) * one_log1m_lambda;
  if (res > Max_long) return Max_long;
  return (uintnat)res;
}

static uintnat next_mt_generate_binom;
/* Simulate a binomial variable of parameters [len] and [lambda].
   This sampling algorithm has running time linear with [len *
   lambda].  We could use more a involved algorithm, but this should
   be good enough since, in the average use case, [lambda] <= 0.01 and
   therefore the generation of the binomial variable is amortized by
   the initialialization of the corresponding block.

   If needed, we could use algorithm BTRS from the paper:
     Hormann, Wolfgang. "The generation of binomial random variates."
     Journal of statistical computation and simulation 46.1-2 (1993), pp101-110.

   Requires [lambda > 0] and [len < Max_long].
 */
static uintnat mt_generate_binom(uintnat len)
{
  uintnat res;
  for(res = 0; next_mt_generate_binom < len; res++)
    next_mt_generate_binom += mt_generate_geom();
  next_mt_generate_binom -= len;
  return res;
}

/**** Interface with the OCaml code. ****/

static void reinit_tracked(void);

CAMLprim value caml_memprof_set(value lv, value szv,
                                value cb_alloc_minor, value cb_alloc_major,
                                value cb_promote,
                                value cb_dealloc_minor, value cb_dealloc_major)
{
  CAMLparam5(lv, szv, cb_alloc_minor, cb_alloc_major, cb_promote);
  CAMLxparam2(cb_dealloc_minor, cb_dealloc_major);
  double l = Double_val(lv);
  intnat sz = Long_val(szv);

  if (sz < 0 || !(l >= 0.) || l > 1.) /* Checks that [l] is not NAN. */
    caml_invalid_argument("caml_memprof_set");

  /* This call to [caml_memprof_set] will discard all the previously
     tracked blocks. We try one last time to call the postponed
     callbacks. */
  if (!caml_memprof_suspended)
    caml_memprof_handle_postponed();

  /* Discard the tracked blocks. */
  reinit_tracked();

  if (!init) {
    int i;
    init = 1;

    mt_index = 624;
    mt_state[0] = 42;
    for(i = 1; i < 624; i++)
      mt_state[i] = 0x6c078965 * (mt_state[i-1] ^ (mt_state[i-1] >> 30)) + i;

    caml_register_generational_global_root(&callback_alloc_minor);
    caml_register_generational_global_root(&callback_alloc_major);
    caml_register_generational_global_root(&callback_promote);
    caml_register_generational_global_root(&callback_dealloc_minor);
    caml_register_generational_global_root(&callback_dealloc_major);
  }

  lambda = l;
  if (l > 0) {
    one_log1m_lambda = l == 1 ? 0 : 1/caml_log1p(-l);
    next_mt_generate_binom = mt_generate_geom();
  }

  caml_memprof_renew_minor_sample();

  callstack_size = sz;

  caml_modify_generational_global_root(&callback_alloc_minor, cb_alloc_minor);
  caml_modify_generational_global_root(&callback_alloc_major, cb_alloc_major);
  caml_modify_generational_global_root(&callback_promote, cb_promote);
  caml_modify_generational_global_root(&callback_dealloc_minor,
                                       cb_dealloc_minor);
  caml_modify_generational_global_root(&callback_dealloc_major,
                                       cb_dealloc_major);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_memprof_set_byt(value* argv, int argn)
{
  CAMLassert(argn == 7);
  return caml_memprof_set(argv[0], argv[1], argv[2], argv[3],
                          argv[4], argv[5], argv[6]);
}

/**** Capturing the call stack *****/

/* This function is called for postponed blocks, so it guarantees
   that the GC is not called. */
static value capture_callstack_postponed(void)
{
  value res;
  uintnat wosize = caml_current_callstack_size(callstack_size);
  /* We do not use [caml_alloc] to make sure the GC will not get called. */
  if (wosize == 0) return Atom(0);
  res = caml_alloc_shr_no_track_noexc(wosize, 0);
  if (res != 0) caml_current_callstack_write(res);
  return res;
}

static value capture_callstack(void)
{
  value res;
  uintnat wosize = caml_current_callstack_size(callstack_size);
  CAMLassert(!caml_memprof_suspended);
  caml_memprof_suspended = 1; /* => no samples in the call stack. */
  res = caml_alloc(wosize, 0);
  caml_memprof_suspended = 0;
  caml_current_callstack_write(res);
  return res;
}

/**** Data structures for tracked blocks. ****/
/* Each tracked block is described in memory by one inhabitant of the
   [struct tracked] structure: */

enum cb_kind { CB_ALLOC, CB_PROMOTE, CB_DEALLOC, CB_NONE };

struct tracked {
  /* Memory block being sampled. */
  value block;

  /* Next and previous elements of the [tracked_old]/[tracked_young] list. */
  struct tracked *next, *prev;

  /* Number of samples in this block. */
  uintnat occurrences;

  /* The header of this block (useful for tag and size). */
  header_t header;

  /* The value returned by the previous callback for this block, or
     the callstack if the alloc callback has not been called yet. */
  value user_data;

  /* Next and previous elements of the [young_user_data] list. */
  struct tracked *next_young_user_data, *prev_young_user_data;

  /* Value of type [enum cb_kind] describing which is the callback to
     be executed/being executed for this block. */
  unsigned int cb_kind : 2;

  /* Whether this block has been initially allocated in the minor heap. */
  unsigned int alloc_young : 1;

  /* Whether this block comes from unmarshalling. */
  unsigned int unmarshalled : 1;

  /* Whether this block has been promoted. Implies [alloc_young]. */
  unsigned int promoted : 1;

  /* Whether this block has been deallocated. */
  unsigned int deallocated : 1;

  /* These Booleans indicates which of the corresponding
     functions/structures have a pointer to this structure, to
     determine when it is safe to free it. */
  unsigned int in_list : 1;
  unsigned int in_do_callback : 1;

  /* If there is a pending callback for this block, this field
     contains the next one in the queue. */
  struct tracked *next_callback_queue;
};

/* We have 3 doubly linked lists for tracked blocks:

   - The linked list of young tracked blocks, rooted at
     [first_tracked_young]. In this list, [block] may be [Val_unit] if
     the allocation callback for this block is still executing in
     [caml_memprof_track_young].

   - The linked list of old and deallocated blocks, rooted at
     [first_tracked_old]. In this list, [block] may be [Val_unit] if
     the block has been deallocated by the deallocation callback has
     not yet finished.

   - The linked list of blocks whose [user_data] field is young, in
     order to efficiently traverse these data during a minor
     collection. It is rooted at [first_young_user_data] and uses the
     fields [next_young_user_data] and [prev_young_user_data].
     All its elements are in one of the two lists above.

  In addition to those, the pending callbacks are stored in a queue
  represented as a singly linked list described further bellow.
 */
static struct tracked *first_tracked_old = NULL, *first_tracked_young = NULL;
static struct tracked *first_young_user_data = NULL;

/* [tracked_set_user_data] writes a value to the [user_data] field,
   and updates the [young_user_data] list accordingly. */
static void tracked_set_user_data(struct tracked *t, value user_data)
{
  value old_user_data = t->user_data;
  int was_young = Is_block(old_user_data) && Is_young(old_user_data);
  int is_young = Is_block(user_data) && Is_young(user_data);
  t->user_data = user_data;
  if(is_young && !was_young) {
    CAMLassert(t->next_young_user_data == NULL &&
               t->prev_young_user_data == NULL);
    if(first_young_user_data != NULL) {
      CAMLassert(first_young_user_data->prev_young_user_data == NULL);
      first_young_user_data->prev_young_user_data = t;
    }
    t->next_young_user_data = first_young_user_data;
    first_young_user_data = t;
  } else if(!is_young && was_young) {
    CAMLassert(t->prev_young_user_data != NULL || t == first_young_user_data);
    if(t->next_young_user_data != NULL)
      t->next_young_user_data->prev_young_user_data = t->prev_young_user_data;
    if(t->prev_young_user_data != NULL)
      t->prev_young_user_data->next_young_user_data = t->next_young_user_data;
    if(t == first_young_user_data)
      first_young_user_data = t->next_young_user_data;
    t->prev_young_user_data = t->next_young_user_data = NULL;
  }
}

/* Creates a new tracked block descriptor, initializes it, and inserts
   it in the [tracked_young]/[tracked_old] list. */
static struct tracked *new_tracked_insert(uintnat occurrences, header_t header,
                                          int unmarshalled, int young,
                                          value block, value user_data)
{
  struct tracked *t, **fst;

  t = (struct tracked*)caml_stat_alloc_noexc(sizeof(struct tracked));
  if(t == NULL) return NULL;

  t->block = block;

  fst = young ? &first_tracked_young : &first_tracked_old;
  t->next = *fst;
  if(*fst != NULL) (*fst)->prev = t;
  *fst = t;
  t->prev = NULL;

  CAMLassert(occurrences > 0);
  t->occurrences = occurrences;
  t->header = header;
  t->user_data = Val_unit;
  t->next_young_user_data = t->prev_young_user_data = NULL;
  t->cb_kind = CB_NONE;
  t->alloc_young = young;
  t->unmarshalled = unmarshalled;
  t->promoted = 0;
  t->deallocated = 0;
  t->in_list = 1;
  t->in_do_callback = 0;
  t->next_callback_queue = NULL;

  tracked_set_user_data(t, user_data);

  return t;
}

static void try_free_tracked(struct tracked *t)
{
  if(!t->in_list && !t->in_do_callback)
    caml_stat_free(t);
}

/* Forward declaration for the sole purpose of the assertion in
   [remove_tracked_and_free]. */
static struct tracked *last_callback_queue;

/* Remove [t] from the tracked lists if it is still there. Assumes it
   is not in the callback queue. */
static void remove_tracked_and_free(struct tracked *t)
{
  CAMLassert(t->next_callback_queue == NULL && t != last_callback_queue);
  if(t->in_list) {
    if(t->next != NULL) t->next->prev = t->prev;
    if(t->prev != NULL) t->prev->next = t->next;
    else CAMLassert(t == first_tracked_old || t == first_tracked_young);
    if(t == first_tracked_young) {
      CAMLassert(t->prev == NULL);
      first_tracked_young = t->next;
    }
    if(t == first_tracked_old)  {
      CAMLassert(t->prev == NULL);
      first_tracked_old = t->next;
    }

    if(t->next_young_user_data != NULL)
      t->next_young_user_data->prev_young_user_data = t->prev_young_user_data;
    if(t->prev_young_user_data != NULL) {
      CAMLassert(t != first_young_user_data);
      t->prev_young_user_data->next_young_user_data = t->next_young_user_data;
    } else if(t == first_young_user_data) {
      first_young_user_data = t->next_young_user_data;
    }

    t->in_list = 0;
  }
  try_free_tracked(t);
}

/**** Handling callbacks. ****/
/* When an event occurs (allocation, promotion or deallocation), we
   cannot immediately call the callback, because we may not be in a
   context where the execution of arbitrary OCaml code is
   allowed. These functions make it possible to register a callback in
   a todo-list so that the call is performed when possible. */

int caml_memprof_to_do = 0;

/* The TODO list is represented as a singly linked list. All its
   elements are members of the [tracked_old]/[tracked_young] list. */
static struct tracked *first_callback_queue = NULL,
  *last_callback_queue = NULL;

/* Register a callback for the given tracked block. If another
   callback for this block is pending or running in another thread,
   then this function does not do anything. In this case, we let
   [do_callback] register this new callback.

   This function does not call the GC. This is important since it is
   called when allocating a block using [caml_alloc_shr]: The new
   block is allocated, but not yet initialized, so that the heap
   invariants are broken. */
static void register_postponed_callback(struct tracked *t, enum cb_kind kind)
{
  CAMLassert(kind != CB_NONE);
  if(t == NULL) return;  /* If the allocation of [t] resulted in OOM. */
  CAMLassert(t->in_list);
  if(t->cb_kind != CB_NONE) return;

  t->cb_kind = kind;

  CAMLassert(t->next_callback_queue == NULL);
  if(last_callback_queue == NULL) {
    CAMLassert(first_callback_queue == NULL);
    first_callback_queue = t;
  } else {
    CAMLassert(first_callback_queue != NULL);
    CAMLassert(last_callback_queue->next_callback_queue == NULL);
    last_callback_queue->next_callback_queue = t;
  }
  last_callback_queue = t;

  if(!caml_memprof_suspended) caml_set_something_to_do();
}

/* Executes the callback assoctiated with the given tracked block, and
   enque the following callbacks if other events occured on this
   block.

   Assumes [t] is in the tracked list, but not in the callback queue.

   Returns 1 if [t] is still in the list after the callback, and 0 if
   it has been dropped and freed.

   When we call [do_callback], we suspend/resume sampling. In order to
   to avoid a systematic unnecessary calls to [caml_check_urgent_gc]
   after each memprof callback, we do not set [caml_something_to_do]
   when resuming. Therefore, any call to [do_callback] has to also
   make sure the postponed queue will be handled fully at some
   point. */
static int do_callback(struct tracked* t)
{
  CAMLparam0();
  CAMLlocal1(sample_info);
  value res;  /* Not a root, can be an exception result. */
  enum cb_kind cb_kind = t->cb_kind;

  CAMLassert(t->in_list);
  CAMLassert(t->next_callback_queue == NULL && last_callback_queue != t);

  caml_memprof_suspended = 1;

  t->in_do_callback = 1;

  switch(cb_kind) {
    case CB_ALLOC:
      sample_info = caml_alloc_small(5, 0);
      Field(sample_info, 0) = Val_long(t->occurrences);
      Field(sample_info, 1) = Val_long(Wosize_hd(t->header));
      Field(sample_info, 2) = Val_long(Tag_hd(t->header));
      Field(sample_info, 3) = Val_long(t->unmarshalled);
      Field(sample_info, 4) = t->user_data;
      res = caml_callback_exn(
          t->alloc_young ? callback_alloc_minor : callback_alloc_major,
          sample_info);
      break;
    case CB_PROMOTE:
      res = caml_callback_exn(callback_promote, t->user_data);
      break;
    case CB_DEALLOC:
      if(t->promoted || !t->alloc_young)
        res = caml_callback_exn(callback_dealloc_major, t->user_data);
      else
        res = caml_callback_exn(callback_dealloc_minor, t->user_data);
      break;
    default: caml_fatal_error("Impossible case in do_callback");
  }

  caml_memprof_suspended = 0;
  t->cb_kind = CB_NONE;

  if(Is_exception_result(res) || cb_kind == CB_DEALLOC ||
     res == Val_long(0) /* None */) {
    t->in_do_callback = 0;
    remove_tracked_and_free(t);

    if(Is_exception_result(res))
      /* We are not necessarily called from `caml_check_urgent_gc`, but
         this is OK to call this regardless of this fact.  */
      caml_raise_in_async_callback(Extract_exception(res));
    else
      CAMLreturn(0);
  }

  if(t->in_list) {
    tracked_set_user_data(t, Field(res, 0));

    if(cb_kind == CB_ALLOC && t->promoted)
      register_postponed_callback(t, CB_PROMOTE);
    else if(t->deallocated)
      register_postponed_callback(t, CB_DEALLOC);

    CAMLreturn(1);
  } else {
    t->in_do_callback = 0;
    try_free_tracked(t);

    CAMLreturn(0);
  }
}

/* Empty the callback queue by running all of them. */
void caml_memprof_handle_postponed(void)
{
  if(caml_memprof_suspended) {
    caml_memprof_to_do = 0;
    return;
  }

  while(first_callback_queue != NULL) {
    struct tracked *t = first_callback_queue;
    first_callback_queue = t->next_callback_queue;
    if(first_callback_queue == NULL) last_callback_queue = NULL;
    t->next_callback_queue = NULL;

    /* If using threads, this call can trigger reentrant calls to
       [caml_memprof_handle_postponed] even though we set
       [caml_memprof_suspended]. Moreover, control flow may leave this
       loop because of an exception. So we need to make sure the data
       structures are in a valid state at this point. */
    do_callback(t);
  }

  caml_memprof_to_do = 0;
  return;
}

/* Reinitialize all the data structures by dropping all the tracked
   blocks. */
static void reinit_tracked(void)
{
  int i;
  for(i = 0; i < 2; i++) {
    struct tracked *t = i == 0 ? first_tracked_old : first_tracked_young;
    while(t != NULL) {
      struct tracked *next_t = t->next;
      t->in_list = 0;
      try_free_tracked(t);
      t = next_t;
    }
  }

  first_tracked_young = first_tracked_old = NULL;
  first_young_user_data = NULL;
  first_callback_queue = last_callback_queue = NULL;
}

/**** Handling roots ****/

/* Called during minor collection. We oldify all the [user_data]
   roots. */
void caml_memprof_oldify_young_roots(void)
{
  struct tracked *t = first_young_user_data;
  while(t != NULL) {
    struct tracked * t_next = t->next_young_user_data;
    CAMLassert(Is_block(t->user_data) && Is_young(t->user_data));
    caml_oldify_one(t->user_data, &t->user_data);
    t->next_young_user_data = t->prev_young_user_data = NULL;
    t = t_next;
  }
  first_young_user_data = NULL;
}

/* Called at the end of minor collection. We check for each oung
   tracked block whether it has been promoted or deallocated. */
void caml_memprof_minor_update(void)
{
  struct tracked *t = first_tracked_young, *t_next;
  while(t != NULL) {
    t_next = t->next;

    /* [t->block] may not point to a block if we are still in the
       process of allocating this new block (i.e., if we are still in
       [caml_memprof_track_young] for this block. */
    if(Is_block(t->block)) {
      CAMLassert(Is_young(t->block));
      if(Hd_val(t->block) == 0) {
        /* Block has been promoted. */
        t->block = Field(t->block, 0);
        t->promoted = 1;
        register_postponed_callback(t, CB_PROMOTE);
      } else {
        /* Block is dead. */
        t->block = Val_unit;
        t->deallocated = 1;
        register_postponed_callback(t, CB_DEALLOC);
      }

      /* This block is now old: we move it from the young list to the
         old list. */
      if(t->prev != NULL) {
        CAMLassert(first_tracked_young != t);
        t->prev->next = t_next;
      } else {
        CAMLassert(first_tracked_young == t);
        first_tracked_young = t_next;
      }
      if(t_next != NULL) t_next->prev = t->prev;

      t->next = first_tracked_old;
      if(first_tracked_old != NULL) {
        CAMLassert(first_tracked_old->prev == NULL);
        first_tracked_old->prev = t;
      }
      first_tracked_old = t;
      t->prev = NULL;
    }
    t = t_next;
  }
}

/* Called by the major GC and the compactor: traverse all the
   [user_data] roots. */
void caml_memprof_do_roots(scanning_action f)
{
  int i;
  for(i = 0; i < 2; i++) {
    struct tracked *t = i == 0 ? first_tracked_old : first_tracked_young;
    while(t != NULL) {
      f(t->user_data, &t->user_data);
      t = t->next;
    }
  }
}

/* Called at the end of a major GC cycle: We check whether a tracked
   block has been deallocated. */
void caml_memprof_update_clean_phase(void)
{
  struct tracked *t = first_tracked_old;
  while(t != NULL) {
    if(Is_block(t->block)) {
      CAMLassert(Is_in_heap(t->block));
      if(Is_white_val(t->block)) {
        /* Block is dead. */
        t->block = Val_unit;
        t->deallocated = 1;
        register_postponed_callback(t, CB_DEALLOC);
      }
    } else {
      CAMLassert(t->block == Val_unit);
      CAMLassert(t->deallocated);
    }
    t = t->next;
  }
}

/* Called by the compactor in order to update the weak references to
   tracked blocks. */
void caml_memprof_invert_tracked(void)
{
  struct tracked *t = first_tracked_old;
  while(t != NULL) {
    caml_invert_root(t->block, &t->block);
    t = t->next;
  }

  t = first_tracked_young;
  while(t != NULL) {
    CAMLassert(!Is_block(t->block));
    t = t->next;
  }
}

/**** Sampling procedures ****/

void caml_memprof_track_alloc_shr(value block)
{
  uintnat occurrences;
  value callstack = 0;
  struct tracked *t;
  CAMLassert(Is_in_heap(block));

  /* This test also makes sure memprof is initialized. */
  if(lambda == 0 || caml_memprof_suspended) return;

  occurrences = mt_generate_binom(Whsize_val(block));
  if(occurrences == 0) return;

  callstack = capture_callstack_postponed();
  if(callstack == 0) return;

  t = new_tracked_insert(occurrences, Hd_val(block), 0, 0, block, callstack);
  register_postponed_callback(t, CB_ALLOC);
}

/* Shifts the next sample in the minor heap by [n] words. Essentially,
   this tells the sampler to ignore the next [n] words of the minor
   heap. */
static void shift_sample(uintnat n)
{
  if(caml_memprof_young_trigger - Caml_state->young_alloc_start > n)
    caml_memprof_young_trigger -= n;
  else
    caml_memprof_young_trigger = Caml_state->young_alloc_start;
  caml_update_young_limit();
}

/* Renew the next sample in the minor heap. This needs to be called
   after each minor sampling and after each minor collection. In
   practice, this is called at each sampling in the minor heap and at
   each minor collection. Extra calls do not change the statistical
   properties of the sampling because of the memorylessness of the
   geometric distribution. */
void caml_memprof_renew_minor_sample(void)
{

  if(lambda == 0) /* No trigger in the current minor heap. */
    caml_memprof_young_trigger = Caml_state->young_alloc_start;
  else {
    uintnat geom = mt_generate_geom();
    if(Caml_state->young_ptr - Caml_state->young_alloc_start < geom)
      /* No trigger in the current minor heap. */
      caml_memprof_young_trigger = Caml_state->young_alloc_start;
    caml_memprof_young_trigger = Caml_state->young_ptr - (geom - 1);
  }

  caml_update_young_limit();
}

/* Called when exceeding the threshold for the next sample in the
   minor heap, from the C code (the handling is different when called
   from natively compiled OCaml code). */
void caml_memprof_track_young(tag_t tag, uintnat wosize, int from_caml)
{
  uintnat whsize = Whsize_wosize(wosize);
  uintnat occurrences;
  struct tracked *t;
  value callstack;
  int still_tracked;

  if(caml_memprof_suspended) {
    caml_memprof_renew_minor_sample();
    return;
  }

  /* If [lambda == 0], then [caml_memprof_young_trigger] should be
     equal to [Caml_state->young_alloc_start]. But this function is only
     called with [Caml_state->young_alloc_start <= Caml_state->young_ptr <
     caml_memprof_young_trigger], which is contradictory. */
  CAMLassert(lambda > 0);

  occurrences = 1 +
    mt_generate_binom(caml_memprof_young_trigger - 1 - Caml_state->young_ptr);

  if(!from_caml) {
    caml_memprof_renew_minor_sample();

    callstack = capture_callstack_postponed();
    if(callstack == 0) return;

    t = new_tracked_insert(occurrences, Make_header(wosize, tag, Caml_white),
                           0, 1, Val_hp(Caml_state->young_ptr), callstack);
    register_postponed_callback(t, CB_ALLOC);

    return;
  }

  /* We need to call the callback for this sampled block. Since the
     callback can potentially allocate, the sampled block will *not*
     be the one pointed to by [caml_memprof_young_trigger]. Instead,
     we remember that we need to sample the next allocated word,
     call the callback and use as a sample the block which will be
     allocated right after the callback. */

  /* Restore the minor heap in a valid state for calling the callback.
     We should not call the GC before these two instructions. */
  Caml_state->young_ptr += whsize;
  caml_memprof_renew_minor_sample();

  /* Execute the enqueued callbacks to preserve their order. */
  caml_memprof_handle_postponed();

  callstack = capture_callstack();
  t = new_tracked_insert(occurrences, Make_header(wosize, tag, Caml_white),
                         0, 1, Val_unit, callstack);
  t->cb_kind = CB_ALLOC;
  still_tracked = do_callback(t);

  /* We can now restore the minor heap in the state needed by
     [Alloc_small_aux]. */
  if(Caml_state->young_ptr - whsize < Caml_state->young_trigger) {
    CAML_INSTR_INT("force_minor/memprof@", 1);
    caml_gc_dispatch();
  }

  /* Re-allocate the block in the minor heap. We should not call the
     GC after this. */
  Caml_state->young_ptr -= whsize;

  /* Make sure this block is not going to be sampled again. */
  shift_sample(whsize);

  /* If the execution of the callback has succeeded, then we start the
     tracking of this block.. */
  if(still_tracked) {
    /* Subtlety: we are actually writing [t->block] with an invalid
       (uninitialized) block. This is correct because the allocation
       and initialization happens right after returning from
       [caml_memprof_track_young]. */
    t->block = Val_hp(Caml_state->young_ptr);
    /* TODO: make sure we are in the young list
       (caml_memprof_minor_update may have "promoted" this block) */
  }

  /* /!\ Since the heap is in an invalid state before initialization,
     very little heap operations are allowed until then. */

  return;
}

void caml_memprof_track_interned(header_t* block, header_t* blockend) {
  header_t *p;
  value callstack = 0;
  struct tracked *t;
  int young = Is_young(Val_hp(block));

  if(lambda == 0 || caml_memprof_suspended)
    return;

  /* We have to select the sampled blocks before sampling them,
     because sampling may trigger GC, and then blocks can escape from
     [block, blockend[. So we use the postponing machinery for
     selecting blocks. [intern.c] will call [check_urgent_gc] which
     will call [caml_memprof_handle_postponed] in turn. */
  p = block;
  while(1) {
    uintnat next_sample = mt_generate_geom();
    header_t *next_sample_p, *next_p;
    if(next_sample > blockend - p)
      break;
    /* [next_sample_p] is the block *following* the next sampled
       block! */
    next_sample_p = p + next_sample;

    while(1) {
      next_p = p + Whsize_hp(p);
      if(next_p >= next_sample_p) break;
      p = next_p;
    }

    if(callstack == 0) callstack = capture_callstack_postponed();
    if(callstack == 0) return;  /* OOM */
    t = new_tracked_insert(mt_generate_binom(next_p - next_sample_p) + 1,
                           Hd_hp(p), 1, young, Val_hp(p), callstack);
    register_postponed_callback(t, CB_ALLOC);

    p = next_p;
  }
}
