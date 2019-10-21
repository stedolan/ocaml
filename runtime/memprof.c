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
#include "caml/printexc.h"

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


static struct {
  struct tracked* entries;
  /* The allocated capacity of the entries array */
  uintnat alloc_len;
  /* The number of active entries. (len <= alloc_len) */
  uintnat len;
  /* There are no young blocks before this position (young <= len) */
  uintnat young;
  /* There are no pending callbacks before this position (callback <= len) */
  uintnat callback;
} tracked;

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
  caml_memprof_handle_postponed();

  /* Discard the tracked blocks. */
  tracked.len = 0;
  tracked.callback = tracked.young = 0;

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

/* During the alloc callback for a minor allocation, the block being
   sampled is not yet allocated. Instead, it's represented as this. */
#define Placeholder_value (Val_long(0x42424242))


struct tracked {
  /* Memory block being sampled. */
  value block;

  /* Number of samples in this block. */
  uintnat occurrences;

  /* The header of this block (useful for tag and size) */
  header_t header;

  /* The value returned by the rpevious callback for this block, or
     the callstack if the alloc callback has not been called yet. */
  value user_data;

  /* Whether this block has been initially allocated in the minor heap. */
  unsigned int alloc_young : 1;

  /* Whether this block comes from unmarshalling. */
  unsigned int unmarshalled : 1;

  /* Whether this block has been promoted. Implies [alloc_young]. */
  unsigned int promoted : 1;

  /* Whether this block has been deallocated. */
  unsigned int deallocated : 1;

  /* Whether the allocation callback has been called. */
  unsigned int cb_alloc : 1;

  /* Whether the promotion callback has been called. */
  unsigned int cb_promote : 1;

  /* Whether the deallocation callback has been called. */
  unsigned int cb_dealloc : 1;

  /* Whether this entry is to be deleted. */
  unsigned int delete : 1;
};

static struct tracked* new_tracked(uintnat occurrences, header_t header,
                                   int unmarshalled, int young,
                                   value block, value user_data)
{
  struct tracked *t;
  if (tracked.len == tracked.alloc_len) {
    if (tracked.alloc_len == 0) tracked.alloc_len = 128;
    else tracked.alloc_len *= 2;
    tracked.entries = caml_stat_resize_noexc(tracked.entries,
      tracked.alloc_len * sizeof(struct tracked));
  }
  t = &tracked.entries[tracked.len++];
  t->block = block;
  t->occurrences = occurrences;
  t->header = header;
  t->user_data = user_data;
  t->alloc_young = young;
  t->unmarshalled = unmarshalled;
  t->promoted = 0;
  t->deallocated = 0;
  t->cb_alloc = t->cb_promote = t->cb_dealloc = 0;
  t->delete = 0;
  return t;
}

static void mark_deleted(struct tracked* t)
{
  t->delete = 1;
  t->user_data = Val_unit;
  t->block = Val_unit;
}

static void check_exn(value res)
{
  if (Is_exception_result(res))
    caml_fatal_uncaught_exception(Extract_exception(res));
}

/* Run any needed callbacks for a given entry. (No-op if none are needed) */
static void run_callbacks(uintnat cur)
{
  CAMLparam0();
  CAMLlocal1(sample_info);
  value res;
  struct tracked* t = &tracked.entries[cur];
  if (t->delete) CAMLreturn0;

  if (!t->cb_alloc) {
    t->cb_alloc = 1;
    CAMLassert(Is_block(t->block)
               || t->block == Placeholder_value
               || t->deallocated);
    sample_info = caml_alloc_small(5, 0);
    Field(sample_info, 0) = Val_long(t->occurrences);
    Field(sample_info, 1) = Val_long(Wosize_hd(t->header));
    Field(sample_info, 2) = Val_long(Tag_hd(t->header));
    Field(sample_info, 3) = Val_long(t->unmarshalled);
    Field(sample_info, 4) = t->user_data;
    res = caml_callback_exn(
        t->alloc_young ? callback_alloc_minor : callback_alloc_major,
        sample_info);
    check_exn(res);
    if (res == Val_long(0)) {
      mark_deleted(t); /* Allocation callback returned None, discard this entry */
      CAMLreturn0;
    } else {
      CAMLassert(Is_block(res) && Tag_val(res) == 0 && Wosize_val(res) == 1);
      t->user_data = Field(res, 0);
      if (cur < tracked.young) tracked.young = cur;
    }
  }

  if (t->promoted && !t->cb_promote) {
    t->cb_promote = 1;
    res = caml_callback_exn(callback_promote, t->user_data);
    check_exn(res);
    if (res == Val_long(0)) {
      mark_deleted(t); /* Promotion callback returned None, discard this entry */
      CAMLreturn0;
    } else {
      CAMLassert(Is_block(res) && Tag_val(res) == 0 && Wosize_val(res) == 1);
      t->user_data = Field(res, 0);
      if (cur < tracked.young) tracked.young = cur;
    }
  }

  if (t->deallocated && !t->cb_dealloc) {
    t->cb_dealloc = 1;
    if(t->promoted || !t->alloc_young)
      res = caml_callback_exn(callback_dealloc_major, t->user_data);
    else
      res = caml_callback_exn(callback_dealloc_minor, t->user_data);
    check_exn(res);
    mark_deleted(t); /* Always discard after a dealloc callback */
    CAMLreturn0;
  }

  CAMLreturn0;
}

void caml_memprof_handle_postponed()
{
  uintnat i = tracked.callback, j, prev_len = tracked.len;
  if (caml_memprof_suspended)
    return;
  caml_memprof_suspended = 1;
  while (tracked.callback < tracked.len) {
    uintnat cur = tracked.callback++;
    if (tracked.callback < i) i = cur;
    run_callbacks(cur);
  }
  /* Nothing should have been added */
  CAMLassert(tracked.len == prev_len);
  /* Remove any deleted entries, updating callback and young */
  j = i;
  for (; i < prev_len; i++) {
    if (tracked.young == i) tracked.young = j;
    if (tracked.callback == i) tracked.callback = j;
    if (!tracked.entries[i].delete)
      tracked.entries[j++] = tracked.entries[i];
  }
  tracked.len = j;
  if (tracked.young == prev_len) tracked.young = j;
  if (tracked.callback == prev_len) tracked.callback = j;
  CAMLassert(tracked.callback <= tracked.len);
  CAMLassert(tracked.young <= tracked.len);
  caml_memprof_suspended = 0;
}

void caml_memprof_oldify_young_roots()
{
  uintnat i;
  for (i = tracked.young; i < tracked.len; i++)
    caml_oldify_one(tracked.entries[i].user_data,
                    &tracked.entries[i].user_data);
}

void caml_memprof_minor_update(void)
{
  uintnat i, found_placeholder = 0;
  for (i = tracked.young; i < tracked.len; i++) {
    struct tracked *t = &tracked.entries[i];
    CAMLassert(Is_block(t->block) || t->delete || t->deallocated ||
               (t->block == Placeholder_value && i == tracked.len - 1));
    if (Is_block(t->block) && Is_young(t->block)) {
      if (Hd_val(t->block) == 0) {
        /* Block has been promoted */
        t->block = Field(t->block, 0);
        t->promoted = 1;
      } else {
        /* Block is dead */
        t->block = Val_unit;
        t->deallocated = 1;
      }
    } else if (t->block == Placeholder_value) {
      CAMLassert(i == tracked.len - 1);
      found_placeholder = 1;
    }
  }
  if (tracked.callback > tracked.young) {
    tracked.callback = tracked.young;
    if (!caml_memprof_suspended) caml_set_something_to_do();
  }
  tracked.young = tracked.len - (found_placeholder ? 1 : 0);
}

void caml_memprof_do_roots(scanning_action f)
{
  uintnat i;
  for (i = 0; i < tracked.len; i++)
    f(tracked.entries[i].user_data, &tracked.entries[i].user_data);
}

void caml_memprof_update_clean_phase()
{
  uintnat i;
  for (i = 0; i < tracked.len; i++) {
    struct tracked *t = &tracked.entries[i];
    if (Is_block(t->block) && !Is_young(t->block)) {
      CAMLassert(Is_in_heap(t->block));
      CAMLassert(!t->alloc_young || t->promoted);
      if (Is_white_val(t->block)) {
        t->block = Val_unit;
        t->deallocated = 1;
      }
    }
  }
  tracked.callback = 0;
  if (!caml_memprof_suspended) caml_set_something_to_do();
}

void caml_memprof_invert_tracked()
{
  uintnat i;
  for (i = 0; i < tracked.len; i++)
    caml_invert_root(tracked.entries[i].block, &tracked.entries[i].block);
}


/**** Sampling procedures ****/

void caml_memprof_track_alloc_shr(value block)
{
  uintnat occurrences;
  value callstack = 0;
  CAMLassert(Is_in_heap(block));

  /* This test also makes sure memprof is initialized. */
  if(lambda == 0 || caml_memprof_suspended) return;

  occurrences = mt_generate_binom(Whsize_val(block));
  if(occurrences == 0) return;

  callstack = capture_callstack_postponed();
  if(callstack == 0) return;

  new_tracked(occurrences, Hd_val(block), 0, 0, block, callstack);
  if (!caml_memprof_suspended) caml_set_something_to_do();
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

    new_tracked(occurrences, Make_header(wosize, tag, Caml_white),
                0, 1, Val_hp(Caml_state->young_ptr), callstack);
    if (!caml_memprof_suspended) caml_set_something_to_do();
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

  caml_memprof_handle_postponed();
  callstack = capture_callstack();
  t = new_tracked(occurrences, Make_header(wosize, tag, Caml_white),
                  0, 1, Placeholder_value, callstack);
  CAMLassert(t == &tracked.entries[tracked.len - 1]);
  caml_memprof_suspended = 1;
  run_callbacks(tracked.len - 1);
  caml_memprof_suspended = 0;

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
  CAMLassert(t == &tracked.entries[tracked.len - 1]);
  if (!t->delete) {
    /* Subtlety: we are actually writing [t->block] with an invalid
       (uninitialized) block. This is correct because the allocation
       and initialization happens right after returning from
       [caml_memprof_track_young]. */
    t->block = Val_hp(Caml_state->young_ptr);
  }

  /* /!\ Since the heap is in an invalid state before initialization,
     very little heap operations are allowed until then. */

  return;
}

void caml_memprof_track_interned(header_t* block, header_t* blockend) {
  header_t *p;
  value callstack = 0;
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
    new_tracked(mt_generate_binom(next_p - next_sample_p) + 1,
                Hd_hp(p), 1, young, Val_hp(p), callstack);
    if (!caml_memprof_suspended) caml_set_something_to_do();
    p = next_p;
  }
}
