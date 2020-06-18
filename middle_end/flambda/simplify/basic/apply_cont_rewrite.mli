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

(** Rewrites applied to [Apply_cont] expressions in order to reflect
    changes in continuation arities consequential to addition or removal of
    parameters. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val print : Format.formatter -> t -> unit

val create
   : original_params:Kinded_parameter.t list
  -> used_params:Kinded_parameter.Set.t
  -> extra_params:Kinded_parameter.t list
  -> extra_args:Continuation_extra_params_and_args.Extra_arg.t list
       Apply_cont_rewrite_id.Map.t
  -> used_extra_params:Kinded_parameter.Set.t
  -> t

val extra_params : t -> Kinded_parameter.t list

val extra_args
   : t
  -> Apply_cont_rewrite_id.t
  -> Continuation_extra_params_and_args.Extra_arg.t list

type rewrite_use_result = private
  | Apply_cont of Flambda.Apply_cont.t
  | Expr of Flambda.Expr.t

val no_rewrite : Flambda.Apply_cont.t -> rewrite_use_result

val rewrite_use
   : t
  -> Apply_cont_rewrite_id.t
  -> Flambda.Apply_cont.t
  -> rewrite_use_result

val rewrite_exn_continuation
   : t
  -> Apply_cont_rewrite_id.t
  -> Exn_continuation.t
  -> Exn_continuation.t

val original_params_arity : t -> Flambda_arity.t

val does_nothing : t -> bool
