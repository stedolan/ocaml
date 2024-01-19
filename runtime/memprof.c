/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*            Jacques-Henri Jourdan, projet Gallium, INRIA Paris          */
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

#include <stdbool.h>
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/memprof.h"
#include "caml/mlvalues.h"
#include "caml/platform.h"

/* Design
 *
 * 1. Data Design
 *
 * 1.1. Configuration
 *
 * A Gc.Memprof.t value (a "profile" from the OCaml point of view) is
 * a block on the OCaml heap containing the profile configuration. As
 * a profile may be shared between threads and domains, keeping it on
 * the OCaml heap allows us not to worry about its liveness - pointers
 * to it from memprof data structures are simply treated as GC roots.
 * The "status" field in this object allows distinct domains to safely
 * `stop` and `discard` (with atomic reads and writes).
 *
 * 1.2. Entries
 *
 * Each block of memory tracked by memprof is represented by an
 * "entry" structure (entry_s, *entry_t). It tracks the state of the
 * block of memory, and its progress through the various callbacks.
 *
 * A resizable table of entry structures is called an "entries" table
 * (entries_s, *entries_t). It tracks ranges of those entries which
 * may (a) be ripe for running a callback, (b) be marked for deletion,
 * or (c) contain pointers to the minor heap (to be scanned in a minor
 * collection). As processing each of these actions proceeds linearly
 * through the table, this tracking is done simply by keeping the
 * lowest possible entry index for each purpose. The code to perform
 * each action (running a callback, evicting a deleted entry, or
 * scanning a pointer) checks whether an entry does require the action
 * before performing it.
 *
 * The entries table also has a pointer to the configuration object on
 * the OCaml heap, for the profile under which all the entries in the
 * table were sampled. This allows callbacks on the table to be run at
 * any later time, regardless of the currently-sampling profile of the
 * particular domain running the callback. A consequence is that all
 * entries in a table must be from the same profile.
 *
 * After a profile is "discarded", entries may still exist for blocks
 * allocated in that profile, but no callbacks will be called for it
 * (those entries themselves will be discarded lazily).
 *
 * There is code for iterating over entries in a table, which is used
 * when scanning for GC roots or updating tables to reflect GC activity
 * (see below).
 *
 * 1.3. Threads
 *
 * The memprof state of a particular systhread is a "thread state"
 * (memprof_thread_s, *memprof_thread_t). It has an entries table, for
 * blocks allocated by this thread whose allocation callback has not
 * yet completed.
 *
 * This thread state structure exists whether or not the systhreads
 * module is initialized (one thread state per domain), and whether or
 * not memprof is running.
 *
 * 1.4. Domains
 *
 * The memprof state of a domain is a "domain state"
 * (memprof_domain_s, *memprof_domain_t). It has an entries table, for
 * blocks allocated in this domain whose allocation callbacks have
 * completed, and for blocks allocated in this domain whose allocating
 * threads have exited before calling the allocation callbacks. If a
 * domain terminates, or starts a new profile, while it still has
 * tracked entries from a previous profile, those tracked entries
 * become "orphaned" (see below).
 *
 * The domain state has a linked list of thread states for all the
 * threads in the domain, and a pointer to the current thread state.
 *
 * This structure exists whether or not memprof is running. A pointer
 * to it is kept in the caml_domain_state.
 *
 * 1.5. Orphans
 *
 * When sampling is stopped for a profile, all domains and threads
 * continue to manage the entry tables for it as before, but without
 * running allocation callbacks. However, if a domain _starts_ a
 * profile while it has entries (tracked blocks) from a previous
 * profile which has not been "discarded", it moves those entries to
 * its "orphans" list - a linked list of entry tables - for subsequent
 * processing.
 *
 * If a domain is terminated, all its current and orphaned entries
 * (and those of its threads) are moved to a global `orphans`
 * list. This list, and its protective lock `orphans_lock`, are the
 * only memprof globals. No domain processes the entries in the global
 * orphans list directly: the first domain to look at the list (either
 * at a collection or when checking for pending callbacks) adopts all
 * entry tables on it into its own orphans list, and then processes
 * them as its own.
 *
 * 2. Synchronisation
 *
 * Mostly threads and domains are free to run callbacks on their own
 * allocated blocks without explicitly synchronising. Care is taken
 * not to assume that the memprof state of any given thread or entry
 * in a domain is preserved outside of memprof code, as another thread
 * in the same domain may run and modify that state, but we assume
 * that the systhreads module effectively serializes entries to
 * memprof within a single domain (for these purposes, entering and
 * returning from a callback is treated as leaving and re-entering
 * memprof code).
 *
 * However, there are some structures shared between domains. The main
 * such structure is the profile configuration object on the Caml
 * heap. The only field written in this object is the status field,
 * used to communicate between domains sharing the profile, when a
 * profile is stopped or discarded. This field is inspected or set
 * atomically by the `Status` and `Set_status` macros. If a profile is
 * found to be discarded (`CONFIG_STATUS_DISCARDED`) then no domain
 * need take any action on it (and we can lazily discard any state
 * from it).
 *
 * The only other data shared between domains is the global orphans
 * list. As noted above, this is protected by a single global lock,
 * `orphans_lock`. Because an entry table only gets onto the global
 * orphans list when its owning domain terminates (at which point all
 * threads of that domain have terminated), and a table is adopted
 * from the global orphans list before being processed, all callbacks
 * and other entry table processing is performed by a thread of the
 * domain which owns the entry table. (and actions of those threads
 * are serialized by `systhreads`).
 *
 * 4. Random Number Generation
 *
 * 4.1. Requirements
 *
 * We sample every word of allocation with the same probability
 * (lambda, usually very small) - a Bernoulli trial. For the
 * allocation of a block on the shared heap, or any allocation from
 * the C runtime, we need to know how many samples we make of that
 * block (usually zero). This is a **binomial random variable**,
 * parameterized by lambda and N (the number of words in the block,
 * including the header).
 *
 * For allocations by Caml on the minor heap, we use the existing GC
 * trigger mechanism, to cause Caml to enter the runtime when "the
 * next sample" is due. The amount of allocation before "the next
 * sample" is a **geometric random variable**, parameterized by
 * lambda.
 *
 * 4.2. Implementation
 *
 * We focus on generating geometric pseudo-random numbers (PRNs), and
 * simulate binomial PRNs for parameters (lambda, N) by counting
 * geometric PRNs for lambda which sum to no more than N.
 *
 * We use a high-quality high-performance 32-bit uniform PRNG
 * (xoshiro128+), with per-domain state vectors. We initialize the
 * per-domain state vector with a low-quality PRNG (SplitMX64), seeded
 * separately for each domain.
 *
 * To convert from a uniform PRN `u` to a geometric PRN `g`, we compute
 *
 *          g = 1 + log(u) / log(1-lambda)
 *
 * where we treat u as uniformly distributed in [0,1]. We pre-compute
 * 1/log(1-lambda) (called `one_log1m_lambda` here), and compute
 * log(u) using a combination of type punning and a 3rd-degree
 * polynomial (see `log_approx()`).
 *
 * For further efficiency we generate geometric PRNs in blocks, and
 * the generating code is designed to be vectorizable.
 */

/* number of random variables in a batch */
#define RAND_BLOCK_SIZE 64

/* type aliases for the hierarchy of structures for managing memprof status. */

typedef struct entry_s entry_s, *entry_t;
typedef struct entries_s entries_s, *entries_t;
typedef struct memprof_domain_s memprof_domain_s, *memprof_domain_t;
typedef struct memprof_thread_s memprof_thread_s, *memprof_thread_t;
typedef struct memprof_orphan_table_s memprof_orphan_table_s,
  *memprof_orphan_table_t;

/* [Gc.Memprof.allocation_source] */

/* At present (since OCaml 5), SRC_MARSHAL can't be produced, because
 * unmarshalling uses the regular allocation functions. */

enum { SRC_NORMAL = 0, SRC_MARSHAL = 1, SRC_CUSTOM = 2 };

/* A memprof configuration is held in an object on the Caml heap, of
 * type Gc.Memprof.t. Here we define getter macros for each field, and
 * a setter macro for the status field (which is updated). */

#define CONFIG_FIELDS 9

#define CONFIG_FIELD_STATUS        0
#define CONFIG_FIELD_LAMBDA        1
#define CONFIG_FIELD_1LOG1ML       2
#define CONFIG_FIELD_FRAMES        3
#define CONFIG_FIELD_ALLOC_MINOR   4
#define CONFIG_FIELD_ALLOC_MAJOR   5
#define CONFIG_FIELD_PROMOTE       6
#define CONFIG_FIELD_DEALLOC_MINOR 7
#define CONFIG_FIELD_DEALLOC_MAJOR 8

#define CONFIG_FIELD_FIRST_CALLBACK CONFIG_FIELD_ALLOC_MINOR
#define CONFIG_FIELD_LAST_CALLBACK CONFIG_FIELD_DEALLOC_MAJOR

#define CONFIG_STATUS_SAMPLING 0
#define CONFIG_STATUS_STOPPED 1
#define CONFIG_STATUS_DISCARDED 2

#define CONFIG_NONE Val_unit

#define Status(config)          Int_val(Field(config, CONFIG_FIELD_STATUS))
#define Sampling(config)        ((config != CONFIG_NONE) && \
                                 (Status(config) == CONFIG_STATUS_SAMPLING))

/* The 'status' field is the only one we ever update. */

#define Set_status(config, stat) \
  Store_field(config, CONFIG_FIELD_STATUS, Val_int(stat))

/* lambda: the fraction of allocated words to sample 0 <= lambda <= 1 */
#define Lambda(config) \
  Double_val(Field(config, CONFIG_FIELD_LAMBDA))

/* 1/ln(1-lambda), pre-computed for use in the geometric RNG */
#define One_log1m_lambda(config) \
  Double_val(Field(config, CONFIG_FIELD_1LOG1ML))

/* The number of frames to record for each allocation site */
#define Callstack_size(config) \
  Int_val(Field(config, CONFIG_FIELD_FRAMES))

/* callbacks */
#define Alloc_minor(config)   Field(config, CONFIG_FIELD_ALLOC_MINOR)
#define Alloc_major(config)   Field(config, CONFIG_FIELD_ALLOC_MAJOR)
#define Promote(config)       Field(config, CONFIG_FIELD_PROMOTE)
#define Dealloc_minor(config) Field(config, CONFIG_FIELD_DEALLOC_MINOR)
#define Dealloc_major(config) Field(config, CONFIG_FIELD_DEALLOC_MAJOR)

/* Callback indexes. "Major" and "minor" are not distinguished here. */

#define CB_NONE          0
#define CB_ALLOC         1
#define CB_PROMOTE       2
#define CB_DEALLOC       3

/* Maximum value of a callback index */
#define CB_MAX           CB_DEALLOC

/* How many bits required for a callback index */
#define CB_BITS          2

/* the mask for a given callback index */
#define CB_MASK(cb) (1 << ((cb) - 1))

/* Structure for each tracked allocation. Six words (with many spare
 * bits in the final word). */

struct entry_s {
  /* Memory block being sampled. This is a weak GC root. Note that
   * during the allocation callback of a block allocated directly by OCaml,
   * this may be a comballoc offset. */
  value block;

  /* The value returned by the previous callback for this block, or
   * the callstack (as a value-tagged pointer to the C heap) if the
   * alloc callback has not been called yet.  This is a strong GC
   * root. */
  value user_data;

  /* Number of samples in this block. */
  size_t samples;

  /* The size of this block. */
  size_t wosize;

  /* The thread currently running a callback for this entry,
   * or NULL if there is none */
  memprof_thread_t running;

  /* Is `block` actually an offset? */
  unsigned int offset : 1;

  /* Was this block initially allocated in the minor heap? */
  unsigned int alloc_young : 1;

  /* The source of the allocation: normal allocations or custom_mem (SRC_*). */
  unsigned int source : 2;

  /* Has this block been promoted? Implies [alloc_young]. */
  unsigned int promoted : 1;

  /* Has this block been deallocated? */
  unsigned int deallocated : 1;

  /* Which callback (CB_*) is currently running for this entry. */
  unsigned int callback : CB_BITS;

  /* A mask of callbacks (1 << (CB_* - 1)) which have been called (not
   * necessarily completed) for this entry. */
  unsigned int callbacks : CB_MAX;

  /* Has this entry been marked for deletion. */
  unsigned int deleted : 1;

  /* There are a number of spare bits here for future expansion,
   * without increasing the size of an entry */
};

/* A resizable array of entry_s entries. */

struct entries_s {
  entry_t t; /* Pointer to array of entry_s structures */
  size_t min_size, size, live; /* array allocation management */

  /* Before this position, the [block] and [user_data] fields both
   * point to the major heap ([young <= live]). */
  size_t young;

  /* There are no blocks to be evicted before this position
   * ([evict <= live]). */
  size_t evict;

  /* There are no pending callbacks before this position
   * ([next <= live]). */
  size_t next;

  /* The profiling configuration under which these blocks were
   * allocated. A strong GC root. */
  value config;
};

/* Per-thread memprof state. */

/* Minimum size of a per-thread entries array */
#define MIN_ENTRIES_THREAD_SIZE 16

/* Minimum size of a per-domain entries array */
#define MIN_ENTRIES_DOMAIN_SIZE 128

struct memprof_thread_s {
  /* [suspended] is used for inhibiting memprof callbacks when
     a callback is running or when an uncaught exception handler is
     called. */
  bool suspended;

  /* The index of the entry in `running_table` for which this thread is
   * currently in a callback */
  size_t running_index;

  /* Pointer to entries table for the current callback, or NULL if not
   * currently running a callback. */
  entries_t running_table;

  /* Entries for blocks allocated in this thread whose alloc callback
   * has not yet been called. */
  entries_s entries;

  /* Per-domain memprof information */
  memprof_domain_t domain;

  /* Linked list of thread structures for this domain. Could use a
   * doubly-linked list for performance, but I haven't measured it. */
  memprof_thread_t next;
};

/* Per-domain memprof state */

struct memprof_domain_s {
  /* The owning domain */
  caml_domain_state *caml_state;

  /* Tracking entries for this domain. In the usual case these are
   * entries allocated by a thread in this domain for which the
   * allocation callback has returned: the entry is then transferred
   * to this per-domain table. However, this table will also include
   * entries for threads in this domain which terminated before
   * calling the allocation callback. */
  entries_s entries;

  /* Orphaned entries - either from previous profiles run in this
   * domain or adopted from terminated domains. */
  memprof_orphan_table_t orphans;

  /* Linked list of threads in this domain */
  memprof_thread_t threads;

  /* The current thread's memprof state. Note that there may not be a
     "current thread". TODO: maybe this shouldn't be nullable.
     Nullability costs us some effort and may be meaningless. See call
     site of caml_memprof_leave_thread() in st_stubs.c. */
  memprof_thread_t current;

  /* ---- random number generation state ---- */

  /* RAND_BLOCK_SIZE separate xoshiro+128 state vectors, defined in this
   * column-major order so that SIMD-aware compilers can parallelize the
   * algorithm. */
  uint32_t xoshiro_state[4][RAND_BLOCK_SIZE];

  /* Array of computed geometric random variables */
  uintnat rand_geom_buff[RAND_BLOCK_SIZE];
  uint32_t rand_pos;

  /* Surplus amount of the current sampling distance, not consumed by
   * previous allocations. Still a legitimate sample of a geometric
   * random variable. */
  uintnat next_rand_geom;

  /* TODO: More fields to add here */
};

struct memprof_orphan_table_s {
  /* An orphaned entries table */
  entries_s entries;

  /* next orphaned table in a linked list. */
  memprof_orphan_table_t next;
};

/* List of all the orphaned entry tables. */
static memprof_orphan_table_t orphans = NULL;

/* lock controlling access to `orphans` variable */
static caml_plat_mutex orphans_lock = CAML_PLAT_MUTEX_INITIALIZER;

/**** Initializing and clearing entries tables ****/

static void entries_init(entries_t es, size_t min_size, value config)
{
  es->t = NULL;
  es->min_size = min_size;
  es->size = 0;
  es->live = 0;
  es->young = 0;
  es->evict = 0;
  es->next = 0;
  es->config = config;
}

static void entries_clear(entries_t es)
{
  /* maintain invariants */
  es->size = es->live = es->young = es->evict = es->next = 0;
  if (es->t) {
    caml_stat_free(es->t);
    es->t = NULL;
  }
  es->config = CONFIG_NONE;
}

/**** Managing entries. ****/

/* When an entries table needs to grow, grow it by this factor */
#define ENTRIES_GROWTH_FACTOR 2

/* Do not shrink an entries table until it is this much too large */
#define ENTRIES_SHRINK_FACTOR 4

/* Reallocate the [es] entries table if it is either too small or too
 * large. [grow] is the number of free cells needed.
 * Returns true if reallocation was not necessary or if it succeeded,
 * and false otherwise. */
static bool ensure_entries(entries_t es, ptrdiff_t grow)
{
  if (es->size == 0 && grow == 0) {
    /* Don't want min_size for an unused table. */
    return true;
  }
  size_t new_live = es->live + grow;
  if (new_live <= es->size &&
     (ENTRIES_SHRINK_FACTOR * new_live >= es->size ||
      es->size == es->min_size)) {
    /* No need to grow or shrink */
    return true;
  }
  size_t new_size = new_live * ENTRIES_GROWTH_FACTOR;
  if (new_size < es->min_size)
    new_size = es->min_size;
  entry_t new_t = caml_stat_resize_noexc(es->t, new_size * sizeof(entry_s));
  if (new_t == NULL) return false;
  es->t = new_t;
  es->size = new_size;
  return true;
}

/* Remove any unwanted entries from [es], updating [es->young] and
 * [es->next] if necessary. Unwanted entries are those marked as
 * `deleted`, and also (if `offsets`) any offset entries. */

static void entries_evict(entries_t es, bool offsets)
{
  size_t i, j;

  /* The obvious linear compaction algorithm */

  if (offsets) { /* offsets may be anywhere */
    j = i = 0;
  } else {
    j = i = es->evict;
  }

  while (i < es->live) {
    bool dead = es->t[i].deleted;
    if (offsets && es->t[i].offset) {
      /* An offset entry, which we ought to evict. We'll never be able
       * to connect this entry to its allocated block, so ideally no
       * callbacks are run for it. However, some callbacks may already
       * have been run. If the allocation callback has been run, but
       * not the deallocation callback, the best we can do is probably
       * to fake deallocating the block, so that alloc/dealloc
       * callback counts correspond. */
      if ((es->t[i].callbacks & CB_MASK(CB_ALLOC)) &&
          ((es->t[i].callbacks & CB_MASK(CB_DEALLOC)) == 0)) {
        es->t[i].offset = false;
        es->t[i].block = Val_unit;
        es->t[i].deallocated = 1;
        if (i < es->next) es->next = i;
      } else {
        dead = true;
      }
    }

    if (dead) {
      /* entry is evicted, but what if a thread is running it?  This
       * can happen specifically if an allocation callback stops a
       * profile and starts a new one: all 'offset' entries are evicted.
       */
      memprof_thread_t runner = es->t[i].running;
      if (runner != NULL) {
        CAMLassert(runner->running_table == es);
        CAMLassert(runner->running_index == i);
        runner->running_table = NULL;
      }
    } else { /* preserve this entry */
      if (i != j) {
        es->t[j] = es->t[i];
        /* if a callback is currently being run on this entry,
         * make sure its index is updated */
        memprof_thread_t runner = es->t[i].running;
        if (runner != NULL) {
          CAMLassert(runner->running_table == es);
          CAMLassert(runner->running_index == i);
          runner->running_index = j;
        }
      }
      ++ j;
    }
    ++ i;
    if (es->young == i) es->young = j;
    if (es->next == i) es->next = j;
  }
  es->evict = es->live = j;
  CAMLassert(es->next <= es->live);
  CAMLassert(es->young <= es->live);

  ensure_entries(es, 0);
}

/* Transfer all entries from one entries table to another. */

static void entries_transfer(entries_t from, entries_t to)
{
  size_t offset = to->live;

  if (from->live == 0) {
    return;
  }

  CAMLassert(to->config == from->config);

  ensure_entries(to, from->live);
  to->live += from->live;

  for (size_t j = 0; j < from->live; ++j) {
    to->t[offset + j] = from->t[j];
    memprof_thread_t thread = from->t[j].running;
    if (thread) { /* unusual */
      CAMLassert(thread->running_table == from);
      CAMLassert(thread->running_index == j);
      thread->running_table = to;
      thread->running_index = offset+j;
    }
  }

  if (to->young == offset) {
    to->young = offset + from->young;
  }
  if (to->evict == offset) {
    to->evict = offset + from->evict;
  }
  if (to->next == offset) {
    to->next = offset + from->next;
  }
  /* Reset `from` to empty, and allow it to shrink */
  ensure_entries(from, -from->live);
  from->young = from->evict = from->next = from->live = 0;
}

/* Return configuration for a thread. If it's been discarded, then
 * reset it to CONFIG_NONE and return that.
*/

static value thread_config(memprof_thread_t thread)
{
  value config = thread->entries.config;
  if ((config != CONFIG_NONE) &&
      (Status(config) == CONFIG_STATUS_DISCARDED)) {
    thread->entries.config = config = CONFIG_NONE;
  }
  return config;
}

/*** Create and destroy orphan tables ***/

/* Orphan any surviving entries from a domain or its threads (after
 * first discarding any deleted and offset entries), onto the domain's
 * orphans list. This copies the domain's table itself, to avoid
 * copying the potentially live array. */

static void orphans_create(memprof_domain_t domain)
{
  /* Transfer any entries from the domain's threads to the domain. */
  memprof_thread_t thread = domain->threads;
  while (thread) {
    entries_transfer(&thread->entries, &domain->entries);
    thread = thread->next;
  }

  entries_t es = &domain->entries;
  entries_evict(es, true); /* remove deleted and offset entries */
  if (!es->live) { /* no live entries */
    entries_clear(es);
  } else { /* Orphan surviving entries */
    memprof_orphan_table_t ot =
      caml_stat_alloc(sizeof(memprof_orphan_table_s));
    if (ot) {
      ot->entries = *es;   /* structure copy */
      ot->next = domain->orphans;
      domain->orphans = ot;
      /* If any thread is running a callback, update its running_table.
       * Can occur if a callback starts a new profile. */
      for (size_t i = 0; i < ot->entries.live; ++i) {
        memprof_thread_t runner = ot->entries.t[i].running;
        if (runner) {
          CAMLassert(runner->running_table == es);
          CAMLassert(runner->running_index == i);
          runner->running_table = &ot->entries;
        }
      }

      /* Clear out es to ensure domain doesn't continue to use it, now
       * that the entries "belong to" the orphans list. Doesn't call
       * entries_clear() because that would free the table. */
      es->t = NULL;
      es->size = es->live = es->young = es->evict = es->next = 0;
    }
  }
}

/* Abandon all a domain's orphans to the global list. */

static void orphans_abandon(memprof_domain_t domain)
{
  /* Find the end of the domain's orphans list */
  memprof_orphan_table_t ot = domain->orphans;
  if (!ot)
    return;

  while(ot->next) {
    ot = ot->next;
  }

  caml_plat_lock(&orphans_lock);
  ot->next = orphans;
  orphans = domain->orphans;
  caml_plat_unlock(&orphans_lock);
  domain->orphans = NULL;
}

/* Adopt all global orphans to the given domain. */

CAMLunused_start
static void orphans_adopt(memprof_domain_t domain)
{
  /* Find the end of the domain's orphans list */
  memprof_orphan_table_t *p = &domain->orphans;
  while(*p) {
    p = &(*p)->next;
  }

  caml_plat_lock(&orphans_lock);
  *p = orphans;
  orphans = NULL;
  caml_plat_unlock(&orphans_lock);
}
CAMLunused_end

/* Destroy an orphan table. */

CAMLunused_start
static void orphans_destroy(memprof_orphan_table_t ot)
{
  entries_clear(&ot->entries);
  caml_stat_free(ot);
}
CAMLunused_end

/**** Statistical sampling ****/

/* We use a low-quality SplitMix64 PRNG to initialize state vectors
 * for a high-quality high-performance 32-bit PRNG (xoshiro128+). That
 * PRNG generates uniform random 32-bit numbers, which we use in turn
 * to generate geometric random numbers parameterized by [lambda].
 * This is all coded in such a way that compilers can readily use SIMD
 * optimisations. */

/* splitmix64 PRNG, used to initialize the xoshiro+128 state
 * vectors. Closely based on the public-domain implementation
 * by Sebastiano Vigna https://xorshift.di.unimi.it/splitmix64.c */

Caml_inline uint64_t splitmix64_next(uint64_t* x)
{
  uint64_t z = (*x += 0x9E3779B97F4A7C15ull);
  z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ull;
  z = (z ^ (z >> 27)) * 0x94D049BB133111EBull;
  return z ^ (z >> 31);
}

/* Initialize all the xoshiro+128 state vectors. */

static void xoshiro_init(memprof_domain_t domain, uint64_t seed)
{
  int i;
  uint64_t splitmix64_state = seed;
  for (i = 0; i < RAND_BLOCK_SIZE; i++) {
    uint64_t t = splitmix64_next(&splitmix64_state);
    domain->xoshiro_state[0][i] = t & 0xFFFFFFFF;
    domain->xoshiro_state[1][i] = t >> 32;
    t = splitmix64_next(&splitmix64_state);
    domain->xoshiro_state[2][i] = t & 0xFFFFFFFF;
    domain->xoshiro_state[3][i] = t >> 32;
  }
}

/* xoshiro128+ PRNG. See Blackman & Vigna; "Scrambled linear
 * pseudorandom number generators"; ACM Trans. Math. Softw., 47:1-32,
 * 2021:
 * "xoshiro128+ is our choice for 32-bit floating-point generation." */

Caml_inline uint32_t xoshiro_next(memprof_domain_t domain, int i)
{
  uint32_t res = domain->xoshiro_state[0][i] + domain->xoshiro_state[3][i];
  uint32_t t = domain->xoshiro_state[1][i] << 9;
  domain->xoshiro_state[2][i] ^= domain->xoshiro_state[0][i];
  domain->xoshiro_state[3][i] ^= domain->xoshiro_state[1][i];
  domain->xoshiro_state[1][i] ^= domain->xoshiro_state[2][i];
  domain->xoshiro_state[0][i] ^= domain->xoshiro_state[3][i];
  domain->xoshiro_state[2][i] ^= t;
  t = domain->xoshiro_state[3][i];
  domain->xoshiro_state[3][i] = (t << 11) | (t >> 21);
  return res;
}

/* Computes [log((y+0.5)/2^32)], up to a relatively good precision,
 * and guarantee that the result is negative, in such a way that SIMD
 * can parallelize it. The average absolute error is very close to
 * 0.
 *
 * Uses a type pun to break y+0.5 into biased exponent `exp` (an
 * integer-valued float in the range [126, 159]) and mantissa `x` (a
 * float in [1,2)). This may discard up to eight low bits of y.
 *
 * Then y+0.5 = x * 2^(exp-127), so if f(x) ~= log(x) - 159*log(2),
 * log((y+0.5)/2^32) ~= f(x) + exp * log(2).
 *
 * We use sollya to find the unique degree-3 polynomial f such that :
 *
 *    - Its average value is that of log(x) - 159*log(2) for x in [1, 2)
 *          (so the sampling has the right mean when lambda is small).
 *    - f(1) = f(2) - log(2), so the approximation is continuous.
 *    - The error at x=1 is -1e-5, so the approximation is always negative.
 *    - The maximum absolute error is minimized in [1, 2) (the actual
 *      maximum absolute error is around 7e-4). */

Caml_inline float log_approx(uint32_t y)
{
  union { float f; int32_t i; } u;
  u.f = y + 0.5f;
  float exp = u.i >> 23;
  u.i = (u.i & 0x7FFFFF) | 0x3F800000;
  float x = u.f;
  return (-111.70172433407f +
          x * (2.104659476859f +
               x * (-0.720478916626f +
                    x * 0.107132064797f)) +
          0.6931471805f * exp);
}

/* This function regenerates [RAND_BLOCK_SIZE] geometric random
 * variables at once. Doing this by batches help us gain performances:
 * many compilers (e.g., GCC, CLang, ICC) will be able to use SIMD
 * instructions to get a performance boost. */

#ifdef SUPPORTS_TREE_VECTORIZE
__attribute__((optimize("tree-vectorize")))
#endif

static void rand_batch(memprof_domain_t domain)
{
  int i;
  float one_log1m_lambda = One_log1m_lambda(domain->entries.config);

  /* Instead of using temporary buffers, we could use one big loop,
     but it turns out SIMD optimizations of compilers are more fragile
     when using larger loops.  */
  uint32_t A[RAND_BLOCK_SIZE];
  float B[RAND_BLOCK_SIZE];

  /* Generate uniform variables in A using the xoshiro128+ PRNG. */
  for (i = 0; i < RAND_BLOCK_SIZE; i++)
    A[i] = xoshiro_next(domain, i);

  /* Generate exponential random variables by computing logarithms. */
  for (i = 0; i < RAND_BLOCK_SIZE; i++)
    B[i] = 1 + log_approx(A[i]) * one_log1m_lambda;

  /* We do the final flooring for generating geometric
     variables. Compilers are unlikely to use SIMD instructions for
     this loop, because it involves a conditional and variables of
     different sizes (32 and 64 bits). */
  for (i = 0; i < RAND_BLOCK_SIZE; i++) {
    double f = B[i];
    CAMLassert (f >= 1);
    /* [Max_long+1] is a power of two => no rounding in the test. */
    if (f >= Max_long+1)
      domain->rand_geom_buff[i] = Max_long;
    else domain->rand_geom_buff[i] = (uintnat)f;
  }

  domain->rand_pos = 0;
}

/* Simulate a geometric random variable of parameter [lambda].
 * The result is clipped in [1..Max_long] */
static uintnat rand_geom(memprof_domain_t domain)
{
  uintnat res;
  CAMLassert(Lambda(domain->entries.config) > 0.);
  if (domain->rand_pos == RAND_BLOCK_SIZE) rand_batch(domain);
  res = domain->rand_geom_buff[domain->rand_pos++];
  CAMLassert(1 <= res && res <= Max_long);
  return res;
}

/* Initialize per-domain PRNG, so we're ready to sample. */

static void rand_init(memprof_domain_t domain)
{
  domain->rand_pos = RAND_BLOCK_SIZE;
  if (domain->entries.config != CONFIG_NONE
      && (Lambda(domain->entries.config) > 0)) {
    /* next_rand_geom can be zero if the next word is to be sampled,
     * but rand_geom always returns a value >= 1. Subtract 1 to correct. */
    domain->next_rand_geom = rand_geom(domain) - 1;
  }
}

/* Simulate a binomial random variable of parameters [len] and
 * [lambda]. This tells us how many times a single block allocation is
 * sampled.  This sampling algorithm has running time linear with [len
 * * lambda].  We could use a more involved algorithm, but this should
 * be good enough since, in the typical use case, [lambda] << 0.01 and
 * therefore the generation of the binomial variable is amortized by
 * the initialialization of the corresponding block.
 *
 * If needed, we could use algorithm BTRS from the paper:
 *  Hormann, Wolfgang. "The generation of binomial random variates."
 *  Journal of statistical computation and simulation 46.1-2 (1993), pp101-110.
 */
CAMLunused_start /* TODO: remove once sampling is merged */
static uintnat rand_binom(memprof_domain_t domain, uintnat len)
{
  uintnat res;
  CAMLassert(Lambda(domain->entries.config) > 0. && len < Max_long);
  for (res = 0; domain->next_rand_geom < len; res++)
    domain->next_rand_geom += rand_geom(domain);
  domain->next_rand_geom -= len;
  return res;
}
CAMLunused_end

/**** Create and destroy thread state structures ****/

static memprof_thread_t thread_create(memprof_domain_t domain)
{
  memprof_thread_t thread = caml_stat_alloc(sizeof(memprof_thread_s));
  if (!thread) {
    return NULL;
  }
  thread->suspended = false;
  thread->running_index = 0;
  thread->running_table = NULL;
  entries_init(&thread->entries, MIN_ENTRIES_THREAD_SIZE,
               domain->entries.config);

  /* attach to domain record */
  thread->domain = domain;
  thread->next = domain->threads;
  domain->threads = thread;

  return thread;
}

/* Destroy a thread data structure.
 * The thread's entries table must be empty. */

static void thread_destroy(memprof_thread_t thread)
{
  memprof_domain_t domain = thread->domain;

  /* A thread cannot be destroyed from inside a callback, as
   * Thread.exit works by raising an exception, taking us out of the
   * callback. */
  CAMLassert (!thread->running_table);
  CAMLassert (thread->entries.live == 0);
  entries_clear(&thread->entries);

  if (domain->current == thread) {
    domain->current = NULL;
  }
  /* remove thread from the per-domain list. Could go faster if we
   * used a doubly-linked list, but that's premature optimisation
   * at this point. */
  memprof_thread_t *p = &domain->threads;
  while (*p != thread) {
    CAMLassert(*p);
    p = &(*p)->next;
  }

  *p = thread->next;

  caml_stat_free(thread);
}

/**** Create and destroy domain state structures ****/

static void domain_destroy(memprof_domain_t domain)
{
  /* Orphan any entries from the domain or its threads, then abandon
   * all orphans to the global table. */
  orphans_create(domain);
  orphans_abandon(domain);

  /* Destroy thread structures */
  memprof_thread_t thread = domain->threads;
  while (thread) {
    memprof_thread_t next = thread->next;
    thread_destroy(thread);
    thread = next;
  }

  caml_stat_free(domain);
}

static memprof_domain_t domain_create(caml_domain_state *caml_state)
{
  memprof_domain_t domain = caml_stat_alloc(sizeof(memprof_domain_s));
  if (!domain) {
    return NULL;
  }

  domain->caml_state = caml_state;
  entries_init(&domain->entries, MIN_ENTRIES_DOMAIN_SIZE, CONFIG_NONE);
  domain->orphans = NULL;
  domain->threads = NULL;
  domain->current = NULL;

  /* create initial thread for domain */
  memprof_thread_t thread = thread_create(domain);
  if (thread) {
    domain->current = thread;
  } else {
    domain_destroy(domain);
    domain = NULL;
  }
  return domain;
}

/**** GC interface ****/

void caml_memprof_scan_roots(scanning_action f,
                             scanning_action_flags fflags,
                             void* fdata,
                             caml_domain_state *state,
                             bool weak,
                             bool global)
{
  memprof_domain_t domain = state->memprof;

  f(fdata, domain->entries.config, &domain->entries.config);
  memprof_thread_t thread = domain->threads;
  while (thread) {
    f(fdata, thread->entries.config, &thread->entries.config);
    thread = thread->next;
  }
}

void caml_memprof_after_minor_gc(caml_domain_state *state, bool global)
{
}

void caml_memprof_after_major_gc(caml_domain_state *state, bool global)
{
}

/**** Running callbacks ****/

value caml_memprof_run_callbacks_exn(void)
{
  return Val_unit;
}

/**** Interface to domain module ***/

void caml_memprof_new_domain(caml_domain_state *parent,
                             caml_domain_state *child)
{
  memprof_domain_t domain = domain_create(child);

  child->memprof = domain;
  /* domain inherits configuration from parent */
  if (domain && parent) {
    domain->current->entries.config =
      domain->entries.config =
      parent->memprof->entries.config;
  }
  /* Initialize RNG */
  xoshiro_init(domain, (uint64_t)child->id);

  /* If already profiling, set up RNG */
  rand_init(domain);
}

void caml_memprof_delete_domain(caml_domain_state *state)
{
  if (!state->memprof) {
    return;
  }
  domain_destroy(state->memprof);
  state->memprof = NULL;
}

/**** Interface with domain action-pending flag ****/

/* If profiling is active in the current domain, and we may have some
 * callbacks pending, set the action pending flag. */

static void set_action_pending_as_needed(memprof_domain_t domain)
{
  if (!domain->current ||
      domain->current->suspended) return;
  if (domain->entries.next < domain->entries.live ||
      domain->current->entries.live > 0)
    caml_set_action_pending(domain->caml_state);
}

/* Set the suspended flag on `domain` to `s`. */

static void update_suspended(memprof_domain_t domain, bool s)
{
  if (domain->current) {
    domain->current->suspended = s;
  }
  caml_memprof_renew_minor_sample(domain->caml_state);
  if (!s) set_action_pending_as_needed(domain);
}

/* Set the suspended flag on the current domain to `s`. */

void caml_memprof_update_suspended(bool s) {
  update_suspended(Caml_state->memprof, s);
}

/**** Sampling procedures ****/

Caml_inline bool sampling(memprof_domain_t domain)
{
  memprof_thread_t thread = domain->current;

  if (thread && !thread->suspended) {
    return Sampling(thread_config(thread));
  }
  return false;
}

/* Renew the next sample in a domain's minor heap. Could race with
 * sampling and profile-stopping code, so do not call from another
 * domain unless the world is stopped. Must be called after each minor
 * sample and after each minor collection. In practice, this is called
 * at each minor sample, at each minor collection, and when sampling
 * is suspended and unsuspended. Extra calls do not change the
 * statistical properties of the sampling because of the
 * memorylessness of the geometric distribution. */

void caml_memprof_renew_minor_sample(caml_domain_state *state)
{
  memprof_domain_t domain = state->memprof;
  value *trigger = state->young_start;
  if (sampling(domain)) {
    uintnat geom = rand_geom(domain);
    if (state->young_ptr - state->young_start > geom) {
      trigger = state->young_ptr - (geom - 1);
    }
  }

  CAMLassert((trigger >= state->young_start) &&
             (trigger <= state->young_ptr));
  state->memprof_young_trigger = trigger;
  caml_reset_young_limit(state);
}

void caml_memprof_track_alloc_shr(value block)
{
}

void caml_memprof_track_custom(value block, mlsize_t bytes)
{
}

void caml_memprof_track_young(uintnat wosize, int from_caml,
                              int nallocs, unsigned char* alloc_lens)
{
}

/**** Interface with systhread. ****/

CAMLexport memprof_thread_t caml_memprof_new_thread(caml_domain_state *state)
{
  return thread_create(state->memprof);
}

CAMLexport memprof_thread_t caml_memprof_main_thread(caml_domain_state *state)
{
  memprof_domain_t domain = state->memprof;
  memprof_thread_t thread = domain->threads;

  /* There should currently be just one thread in this domain */
  CAMLassert(thread);
  CAMLassert(thread->next == NULL);
  return thread;
}

CAMLexport void caml_memprof_delete_thread(memprof_thread_t thread)
{
  entries_transfer(&thread->entries, &thread->domain->entries);
  thread_destroy(thread);
}

CAMLexport void caml_memprof_enter_thread(memprof_thread_t thread)
{
  thread->domain->current = thread;
  update_suspended(thread->domain, thread->suspended);
}

/**** Interface to OCaml ****/

CAMLprim value caml_memprof_start(value lv, value szv, value tracker)
{
  CAMLparam3(lv, szv, tracker);
  CAMLlocal1(one_log1m_lambda_v);

  double lambda = Double_val(lv);
  intnat sz = Long_val(szv);

  /* Checks that [lambda] is within range (and not NaN). */
  if (sz < 0 || !(lambda >= 0.) || lambda > 1.)
    caml_invalid_argument("Gc.Memprof.start");

  memprof_domain_t domain = Caml_state->memprof;

  if (Sampling(thread_config(domain->current))) {
    caml_failwith("Gc.Memprof.start: already started.");
  }

  /* Orphan any surviving tracking entries from a previous profile. */
  orphans_create(domain);

  double one_log1m_lambda = lambda == 1.0 ? 0.0 : 1.0/caml_log1p(-lambda);
  one_log1m_lambda_v = caml_copy_double(one_log1m_lambda);

  value config = caml_alloc_shr(CONFIG_FIELDS, 0);
  caml_initialize(&Field(config, CONFIG_FIELD_STATUS),
                  Val_int(CONFIG_STATUS_SAMPLING));
  caml_initialize(&Field(config, CONFIG_FIELD_LAMBDA), lv);
  caml_initialize(&Field(config, CONFIG_FIELD_1LOG1ML), one_log1m_lambda_v);
  caml_initialize(&Field(config, CONFIG_FIELD_FRAMES), szv);
  for (int i = CONFIG_FIELD_FIRST_CALLBACK;
       i <= CONFIG_FIELD_LAST_CALLBACK; ++i) {
    caml_initialize(&Field(config, i), Field(tracker,
                                             i - CONFIG_FIELD_FIRST_CALLBACK));
  }
  CAMLassert(domain->entries.live == 0);

  /* Set config pointers of the domain and all its threads */
  domain->entries.config = config;
  memprof_thread_t thread = domain->threads;
  while (thread) {
    CAMLassert(thread->entries.live == 0);
    thread->entries.config = config;
    thread = thread->next;
  }

  /* reset PRNG, generate first batch of random numbers. */
  rand_init(domain);

  caml_memprof_renew_minor_sample(Caml_state);

  CAMLreturn(config);
}

CAMLprim value caml_memprof_stop(value unit)
{
  memprof_domain_t domain = Caml_state->memprof;
  value config = thread_config(domain->current);

  if (config == CONFIG_NONE || Status(config) != CONFIG_STATUS_SAMPLING) {
    caml_failwith("Gc.Memprof.stop: no profile running.");
  }
  Set_status(config, CONFIG_STATUS_STOPPED);

  caml_memprof_renew_minor_sample(Caml_state);

  return Val_unit;
}

CAMLprim value caml_memprof_discard(value config)
{
  uintnat status = Status(config);
  CAMLassert((status == CONFIG_STATUS_STOPPED) ||
             (status == CONFIG_STATUS_SAMPLING) ||
             (status == CONFIG_STATUS_DISCARDED));

  switch (status) {
  case CONFIG_STATUS_STOPPED: /* correct case */
    break;
  case CONFIG_STATUS_SAMPLING:
    caml_failwith("Gc.Memprof.discard: profile not stopped.");
  case CONFIG_STATUS_DISCARDED:
    caml_failwith("Gc.Memprof.discard: profile already discarded.");
  }

  Set_status(config, CONFIG_STATUS_DISCARDED);

  return Val_unit;
}
