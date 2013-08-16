/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* To walk the memory roots for garbage collection */

#include "finalise.h"
#include "globroots.h"
#include "major_gc.h"
#include "memory.h"
#include "minor_gc.h"
#include "misc.h"
#include "mlvalues.h"
#include "roots.h"
#include "stacks.h"

CAMLexport struct caml__roots_block *caml_local_roots = NULL;

CAMLexport void (*caml_scan_roots_hook) (scanning_action f) = NULL;

/* FIXME should rename to [caml_oldify_young_roots] and synchronise with
   asmrun/roots.c */
/* Call the scanning action on (at least) all the roots that point to the minor
   heap. */
void caml_do_young_roots (scanning_action f)
{
  register value * sp;
  struct caml__roots_block *lr;
  intnat i, j;

  /* The stack and the local C roots*/
  caml_do_local_roots (f, caml_extern_sp, caml_stack_high, caml_local_roots);
  /* Globals */
  f(caml_global_data, &caml_global_data);
  /* Global C roots */
  caml_scan_global_young_roots(f);
  /* Finalised values */
  caml_final_do_young_roots (f);
  /* Hook */
  if (caml_scan_roots_hook != NULL) (*caml_scan_roots_hook)(f);
}

/* Call [caml_darken] on all roots */

void caml_darken_all_roots (void)
{
  caml_do_roots (caml_darken);
}

void caml_do_roots (scanning_action f)
{
  /* Global variables */
  f(caml_global_data, &caml_global_data);
  /* The stack and the local C roots */
  caml_do_local_roots(f, caml_extern_sp, caml_stack_high, caml_local_roots);
  /* Global C roots */
  caml_scan_global_roots(f);
  /* Finalised values */
  caml_final_do_strong_roots (f);
  /* Hook */
  if (caml_scan_roots_hook != NULL) (*caml_scan_roots_hook)(f);
}

CAMLexport void caml_do_local_roots (scanning_action f, value *stack_low,
                                     value *stack_high,
                                     struct caml__roots_block *local_roots)
{
  register value * sp;
  struct caml__roots_block *lr;
  int i, j;

  for (sp = stack_low; sp < stack_high; sp++) {
    f (*sp, sp);
  }
  for (lr = local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        f (*sp, sp);
      }
    }
  }
}
