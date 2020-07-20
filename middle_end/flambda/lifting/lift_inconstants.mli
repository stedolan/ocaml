(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Attempt to statically-allocate values whose structure can be deduced
    by examining the types of the parameters of continuations occurring
    at toplevel. *)

[@@@ocaml.warning "+a-30-40-41-42"]

val lift_via_reification_of_continuation_param_types
   : Downwards_acc.t
  -> params:Kinded_parameter.List.t
  -> extra_params_and_args:Continuation_extra_params_and_args.t
  -> handler:Flambda.Expr.t
  -> Downwards_acc.t * Flambda.Expr.t

type reify_primitive_at_toplevel_result =
  | Lift of {
    dacc : Downwards_acc.t;
    symbol : Symbol.t;
    static_const : Flambda.Static_const.t;
  }
  | Shared of Symbol.t
  | Cannot_reify

val reify_primitive_at_toplevel
   : Downwards_acc.t
  -> Var_in_binding_pos.t
  -> Flambda_type.t
  -> reify_primitive_at_toplevel_result
