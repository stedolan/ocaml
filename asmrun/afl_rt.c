/*
   american fuzzy lop - LLVM instrumentation bootstrap
   ---------------------------------------------------

   Written by Laszlo Szekeres <lszekeres@google.com> and
              Michal Zalewski <lcamtuf@google.com>

   LLVM integration design comes from Laszlo Szekeres.

   Copyright 2015 Google Inc. All rights reserved.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at:

     http://www.apache.org/licenses/LICENSE-2.0

   This code is the rewrite of afl-as.h's main_payload.

*/
/*
   american fuzzy lop - type definitions and minor macros
   ------------------------------------------------------

   Written and maintained by Michal Zalewski <lcamtuf@google.com>

   Copyright 2013, 2014, 2015 Google Inc. All rights reserved.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at:

     http://www.apache.org/licenses/LICENSE-2.0

 */

#ifndef _HAVE_TYPES_H
#define _HAVE_TYPES_H

#include <stdint.h>
#include <stdlib.h>

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;

/*

   Ugh. There is an unintended compiler / glibc #include glitch caused by
   combining the u64 type an %llu in format strings, necessitating a workaround.

   In essence, the compiler is always looking for 'unsigned long long' for %llu.
   On 32-bit systems, the u64 type (aliased to uint64_t) is expanded to
   'unsigned long long' in <bits/types.h>, so everything checks out.

   But on 64-bit systems, it is #ifdef'ed in the same file as 'unsigned long'.
   Now, it only happens in circumstances where the type happens to have the
   expected bit width, *but* the compiler does not know that... and complains
   about 'unsigned long' being unsafe to pass to %llu.

 */

#ifdef __x86_64__
typedef unsigned long long u64;
#else
typedef uint64_t u64;
#endif /* ^sizeof(...) */

typedef int8_t   s8;
typedef int16_t  s16;
typedef int32_t  s32;
typedef int64_t  s64;

#ifndef MIN
#  define MIN(_a,_b) ((_a) > (_b) ? (_b) : (_a))
#  define MAX(_a,_b) ((_a) > (_b) ? (_a) : (_b))
#endif /* !MIN */

#define SWAP16(_x) ({ \
    u16 _ret = (_x); \
    (u16)((_ret << 8) | (_ret >> 8)); \
  })

#define SWAP32(_x) ({ \
    u32 _ret = (_x); \
    (u32)((_ret << 24) | (_ret >> 24) | \
          ((_ret << 8) & 0x00FF0000) | \
          ((_ret >> 8) & 0x0000FF00)); \
  })

#define R(x) (random() % (x))

#define STRINGIFY_INTERNAL(x) #x
#define STRINGIFY(x) STRINGIFY_INTERNAL(x)

#define MEM_BARRIER() \
  asm volatile("" ::: "memory")

#endif /* ! _HAVE_TYPES_H */


/*
   american fuzzy lop - vaguely configurable bits
   ----------------------------------------------

   Written and maintained by Michal Zalewski <lcamtuf@google.com>

   Copyright 2013, 2014, 2015 Google Inc. All rights reserved.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at:

     http://www.apache.org/licenses/LICENSE-2.0

 */

#ifndef _HAVE_CONFIG_H
#define _HAVE_CONFIG_H


/******************************************************
 *                                                    *
 *  Settings that may be of interest to power users:  *
 *                                                    *
 ******************************************************/

/* Comment out to disable terminal colors: */

#define USE_COLOR

/* Comment out to disable fancy ANSI boxes and use poor man's 7-bit UI: */

#define FANCY_BOXES

/* Default timeout for fuzzed code (milliseconds): */

#define EXEC_TIMEOUT        1000

/* Timeout rounding factor when auto-scaling (milliseconds): */

#define EXEC_TM_ROUND       20

/* Default memory limit for child process (MB): */

#ifndef __x86_64__ 
#  define MEM_LIMIT         25
#else
#  define MEM_LIMIT         50
#endif /* ^!__x86_64__ */

/* Default memory limit when running in QEMU mode (MB): */

#define MEM_LIMIT_QEMU      200

/* Number of calibration cycles per every new test case (and for test
   cases that show variable behavior): */

#define CAL_CYCLES          10
#define CAL_CYCLES_LONG     40

/* The same, but when AFL_NO_VAR_CHECK is set in the environment: */

#define CAL_CYCLES_NO_VAR   4

/* Number of subsequent hangs before abandoning an input file: */

#define HANG_LIMIT          250

/* Maximum number of unique hangs or crashes to record: */

#define KEEP_UNIQUE_HANG    500
#define KEEP_UNIQUE_CRASH   5000

/* Baseline number of random tweaks during a single 'havoc' stage: */

#define HAVOC_CYCLES        5000

/* Maximum multiplier for the above (should be a power of two, beware
   of 32-bit int overflows): */

#define HAVOC_MAX_MULT      16

/* Absolute minimum number of havoc cycles (after all adjustments): */

#define HAVOC_MIN           10

/* Maximum stacking for havoc-stage tweaks. The actual value is calculated
   like this: 

   n = random between 1 and HAVOC_STACK_POW2
   stacking = 2^n

   In other words, the default (n = 7) produces 2, 4, 8, 16, 32, 64, or
   128 stacked tweaks: */

#define HAVOC_STACK_POW2    7

/* Caps on block sizes for cloning and deletion operations. Each of these
   ranges has a 33% probability of getting picked, except for the first
   two cycles where smaller blocks are favored: */

#define HAVOC_BLK_SMALL     32
#define HAVOC_BLK_MEDIUM    128
#define HAVOC_BLK_LARGE     1500

/* Probabilities of skipping non-favored entries in the queue, expressed as
   percentages: */

#define SKIP_TO_NEW_PROB    99 /* ...when there are new, pending favorites */
#define SKIP_NFAV_OLD_PROB  95 /* ...no new favs, cur entry already fuzzed */
#define SKIP_NFAV_NEW_PROB  75 /* ...no new favs, cur entry not fuzzed yet */

/* Splicing cycle count: */

#define SPLICE_CYCLES       20

/* Nominal per-splice havoc cycle length: */

#define SPLICE_HAVOC        500

/* Maximum offset for integer addition / subtraction stages: */

#define ARITH_MAX           35

/* Limits for the test case trimmer. The absolute minimum chunk size; and
   the starting and ending divisors for chopping up the input file: */

#define TRIM_MIN_BYTES      4
#define TRIM_START_STEPS    16
#define TRIM_END_STEPS      1024

/* Maximum size of input file, in bytes (keep under 100MB): */

#define MAX_FILE            (1 * 1024 * 1024)

/* The same, for the test case minimizer: */

#define TMIN_MAX_FILE       (10 * 1024 * 1024)

/* Block normalization steps for afl-tmin: */

#define TMIN_SET_MIN_SIZE   4
#define TMIN_SET_STEPS      128

/* Maximum dictionary token size (-x), in bytes: */

#define MAX_DICT_FILE       128

/* Length limits for auto-detected dictionary tokens: */

#define MIN_AUTO_EXTRA      3
#define MAX_AUTO_EXTRA      32

/* Maximum number of user-specified dictionary tokens to use in deterministic
   steps; past this point, the "extras/user" step will be still carried out,
   but with proportionally lower odds: */

#define MAX_DET_EXTRAS      200

/* Maximum number of auto-extracted dictionary tokens to actually use in fuzzing
   (first value), and to keep in memory as candidates. The latter should be much
   higher than the former. */

#define USE_AUTO_EXTRAS     50
#define MAX_AUTO_EXTRAS     (USE_AUTO_EXTRAS * 10)

/* Scaling factor for the effector map used to skip some of the more
   expensive deterministic steps. The actual divisor is set to
   2^EFF_MAP_SCALE2 bytes: */

#define EFF_MAP_SCALE2      3

/* Minimum input file length at which the effector logic kicks in: */

#define EFF_MIN_LEN         128

/* Maximum effector density past which everything is just fuzzed
   unconditionally (%): */

#define EFF_MAX_PERC        90

/* UI refresh frequency (Hz): */

#define UI_TARGET_HZ        5

/* Fuzzer stats file and plot update intervals (sec): */

#define STATS_UPDATE_SEC    60
#define PLOT_UPDATE_SEC     5

/* Smoothing divisor for CPU load and exec speed stats (1 - no smoothing). */

#define AVG_SMOOTHING       16

/* Sync interval (every n havoc cycles): */

#define SYNC_INTERVAL       5

/* Output directory reuse grace period (minutes): */

#define OUTPUT_GRACE        25

/* Uncomment to use simple file names (id_NNNNNN): */

// #define SIMPLE_FILES

/* List of interesting values to use in fuzzing. */

#define INTERESTING_8 \
  -128,          /* Overflow signed 8-bit when decremented  */ \
  -1,            /*                                         */ \
   0,            /*                                         */ \
   1,            /*                                         */ \
   16,           /* One-off with common buffer size         */ \
   32,           /* One-off with common buffer size         */ \
   64,           /* One-off with common buffer size         */ \
   100,          /* One-off with common buffer size         */ \
   127           /* Overflow signed 8-bit when incremented  */

#define INTERESTING_16 \
  -32768,        /* Overflow signed 16-bit when decremented */ \
  -129,          /* Overflow signed 8-bit                   */ \
   128,          /* Overflow signed 8-bit                   */ \
   255,          /* Overflow unsig 8-bit when incremented   */ \
   256,          /* Overflow unsig 8-bit                    */ \
   512,          /* One-off with common buffer size         */ \
   1000,         /* One-off with common buffer size         */ \
   1024,         /* One-off with common buffer size         */ \
   4096,         /* One-off with common buffer size         */ \
   32767         /* Overflow signed 16-bit when incremented */

#define INTERESTING_32 \
  -2147483648LL, /* Overflow signed 32-bit when decremented */ \
  -100663046,    /* Large negative number (endian-agnostic) */ \
  -32769,        /* Overflow signed 16-bit                  */ \
   32768,        /* Overflow signed 16-bit                  */ \
   65535,        /* Overflow unsig 16-bit when incremented  */ \
   65536,        /* Overflow unsig 16 bit                   */ \
   100663045,    /* Large positive number (endian-agnostic) */ \
   2147483647    /* Overflow signed 32-bit when incremented */

/***********************************************************
 *                                                         *
 *  Really exotic stuff you probably don't want to touch:  *
 *                                                         *
 ***********************************************************/

/* Call count interval between reseeding the libc PRNG from /dev/urandom: */

#define RESEED_RNG          10000

/* Maximum line length passed from GCC to 'as' and used for parsing
   configuration files: */

#define MAX_LINE            8192

/* Environment variable used to pass SHM ID to the called program. */

#define SHM_ENV_VAR         "__AFL_SHM_ID"

/* Other less interesting, internal-only variables. */

#define CLANG_ENV_VAR       "__AFL_CLANG_MODE"
#define AS_LOOP_ENV_VAR     "__AFL_AS_LOOPCHECK"
#define PERSIST_ENV_VAR     "__AFL_PERSISTENT"
#define DEFER_ENV_VAR       "__AFL_DEFER_FORKSRV"

/* In-code signatures for deferred and persistent mode. */

#define PERSIST_SIG         "##SIG_AFL_PERSISTENT##"
#define DEFER_SIG           "##SIG_AFL_DEFER_FORKSRV##"

/* Distinctive bitmap signature used to indicate failed execution: */

#define EXEC_FAIL_SIG       0xfee1dead

/* Distinctive exit code used to indicate MSAN trip condition: */

#define MSAN_ERROR          86

/* Designated file descriptors for forkserver commands (the application will
   use FORKSRV_FD and FORKSRV_FD + 1): */

#define FORKSRV_FD          198

/* Fork server init timeout multiplier: we'll wait the user-selected
   timeout plus this much for the fork server to spin up. */

#define FORK_WAIT_MULT      10

/* Calibration timeout adjustments, to be a bit more generous when resuming
   fuzzing sessions or trying to calibrate already-added internal finds.
   The first value is a percentage, the other is in milliseconds: */

#define CAL_TMOUT_PERC      125
#define CAL_TMOUT_ADD       50

/* Number of chances to calibrate a case before giving up: */

#define CAL_CHANCES         3

/* Map size for the traced binary (2^MAP_SIZE_POW2). Must be greater than
   2; you probably want to keep it under 18 or so for performance reasons
   (adjusting AFL_INST_RATIO when compiling is probably a better way to solve
   problems with complex programs). You need to recompile the target binary
   after changing this - otherwise, SEGVs may ensue. */

#define MAP_SIZE_POW2       16
#define MAP_SIZE            (1 << MAP_SIZE_POW2)

/* Maximum allocator request size (keep well under INT_MAX): */

#define MAX_ALLOC           0x40000000

/* A made-up hashing seed: */

#define HASH_CONST          0xa5b35705

/* Constants for afl-gotcpu to control busy loop timing: */

#define  CTEST_TARGET_MS    5000
#define  CTEST_BUSY_CYCLES  (10 * 1000 * 1000)

/* Uncomment this to use inferior block-coverage-based instrumentation. Note
   that you need to recompile the target binary for this to have any effect: */

// #define COVERAGE_ONLY

/* Uncomment this to ignore hit counts and output just one bit per tuple.
   As with the previous setting, you will need to recompile the target
   binary: */

// #define SKIP_COUNTS

/* Uncomment this to use instrumentation data to record newly discovered paths,
   but do not use them as seeds for fuzzing. This is useful for conveniently
   measuring coverage that could be attained by a "dumb" fuzzing algorithm: */

// #define IGNORE_FINDS

#endif /* ! _HAVE_CONFIG_H */



#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <assert.h>

#include <sys/mman.h>
#include <sys/shm.h>
#include <sys/wait.h>
#include <sys/types.h>


/* Globals needed by the injected instrumentation. The __afl_area_initial region
   is used for instrumentation output before __afl_map_shm() has a chance to run.
   It will end up as .comm, so it shouldn't be too wasteful. */

u8  __afl_area_initial[MAP_SIZE];
u8* __afl_area_ptr = __afl_area_initial;
u16 __afl_prev_loc;


/* Running in persistent mode? */

static u8 is_persistent;


/* SHM setup. */

static void __afl_map_shm(void) {

  u8 *id_str = getenv(SHM_ENV_VAR);

  /* If we're running under AFL, attach to the appropriate region, replacing the
     early-stage __afl_area_initial region that is needed to allow some really
     hacky .init code to work correctly in projects such as OpenSSL. */

  if (id_str) {

    u32 shm_id = atoi(id_str);

    __afl_area_ptr = shmat(shm_id, NULL, 0);

    /* Whooooops. */

    if (__afl_area_ptr == (void *)-1) _exit(1);

    /* Write something into the bitmap so that even with low AFL_INST_RATIO,
       our parent doesn't give up on us. */

    __afl_area_ptr[0] = 1;

  }

}


/* Fork server logic. */

static void __afl_start_forkserver(void) {

  static u8 tmp[4];
  s32 child_pid;

  u8  child_stopped = 0;

  /* Phone home and tell the parent that we're OK. If parent isn't there,
     assume we're not running in forkserver mode and just execute program. */

  if (write(FORKSRV_FD + 1, tmp, 4) != 4) return;

  while (1) {

    u32 was_killed;
    int status;

    /* Wait for parent by reading from the pipe. Abort if read fails. */

    if (read(FORKSRV_FD, &was_killed, 4) != 4) _exit(1);

    /* If we stopped the child in persistent mode, but there was a race
       condition and afl-fuzz already issued SIGKILL, write off the old
       process. */

    if (child_stopped && was_killed) {
      child_stopped = 0;
      if (waitpid(child_pid, &status, 0) < 0) _exit(1);
    }

    if (!child_stopped) {

      /* Once woken up, create a clone of our process. */

      child_pid = fork();
      if (child_pid < 0) _exit(1);

      /* In child process: close fds, resume execution. */

      if (!child_pid) {

        close(FORKSRV_FD);
        close(FORKSRV_FD + 1);
        return;
  
      }

    } else {

      /* Special handling for persistent mode: if the child is alive but
         currently stopped, simply restart it with SIGCONT. */

      kill(child_pid, SIGCONT);
      child_stopped = 0;

    }

    /* In parent process: write PID to pipe, then wait for child. */

    if (write(FORKSRV_FD + 1, &child_pid, 4) != 4) _exit(1);

    if (waitpid(child_pid, &status, is_persistent ? WUNTRACED : 0) < 0)
      _exit(1);

    /* In persistent mode, the child stops itself with SIGSTOP to indicate
       a successful run. In this case, we want to wake it up without forking
       again. */

    if (WIFSTOPPED(status)) child_stopped = 1;

    /* Relay wait status to pipe, then loop back. */

    if (write(FORKSRV_FD + 1, &status, 4) != 4) _exit(1);

  }

}


/* A simplified persistent mode handler, used as explained in README.llvm. */

int __afl_persistent_loop(unsigned int max_cnt) {

  static u8  first_pass = 1;
  static u32 cycle_cnt;

  if (first_pass) {

    cycle_cnt  = max_cnt;
    first_pass = 0;
    return 1;

  }

  if (is_persistent && --cycle_cnt) {

    raise(SIGSTOP);
    return 1;

  } else return 0;

}


/* This one can be called from user code when deferred forkserver mode
    is enabled. */

void __afl_manual_init(void) {

  static u8 init_done;

  if (!init_done) {

    __afl_map_shm();
    __afl_start_forkserver();
    init_done = 1;

  }

}


/* Proper initialization routine. */

__attribute__((constructor(0))) void __afl_auto_init(void) {

  is_persistent = !!getenv(PERSIST_ENV_VAR);

  if (getenv(DEFER_ENV_VAR)) return;

  __afl_manual_init();

}


