(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Sort a group of lifted constants into an order suitable for binding by
    one or more [Let_symbol] bindings.  This includes grouping together sets
    of closures with recursion between them (c.f. the
    [Let_symbol_expr.Bound_symbol.Sets_of_closures] constructor). *)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

type result = private {
  bindings_outermost_last : LC.t list;
}

val empty_result : result

(** The [Name_occurrences.t] values specify extra "hidden" dependencies of the
    associated constant that must be taken into account.  The corresponding
    environment must be provided for lookup of such names, since [LC.t]
    values may contain multiple environments, so it wouldn't be clear which
    one to choose. *)
val sort
   : fold_over_lifted_constants:(
          init:(CIS.Set.t CIS.Map.t * LC.t CIS.Map.t)
       -> f:(CIS.Set.t CIS.Map.t * LC.t CIS.Map.t
         -> LC.t * (DE.t * Name_occurrences.t) option
         -> CIS.Set.t CIS.Map.t * LC.t CIS.Map.t)
       -> CIS.Set.t CIS.Map.t * LC.t CIS.Map.t)
  -> result
