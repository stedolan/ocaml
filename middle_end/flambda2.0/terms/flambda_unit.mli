(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The Flambda representation of a single compilation unit's code. *)

[@@@ocaml.warning "+a-30-40-41-42"]

type t

(** Perform well-formedness checks on the unit. *)
val invariant : t -> unit

(** Print a unit to a formatter. *)
val print : Format.formatter -> t -> unit

(** Create a unit. *)
val create
   : return_continuation:Continuation.t
  -> exn_continuation:Continuation.t
  -> body:Flambda.Expr.t
  -> t

val return_continuation : t -> Continuation.t

val exn_continuation : t -> Continuation.t

(** All closure variables used in the given unit. *)
val used_closure_vars : t -> Var_within_closure.Set.t

val body : t -> Flambda.Expr.t

val iter_sets_of_closures
   : t
  -> f:(closure_symbols:Symbol.t Closure_id.Map.t option
     -> Flambda.Set_of_closures.t
     -> unit)
  -> unit
