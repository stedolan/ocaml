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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(** Print a upwards accumulator to a formatter. *)
val print : Format.formatter -> t -> unit

val create : Simplify_envs.Upwards_env.t -> Downwards_acc.t -> t

(** Extract the environment component of the given upwards accumulator. *)
val uenv : t -> Simplify_envs.Upwards_env.t

val code_age_relation : t -> Code_age_relation.t

(** Return the lifted constants that still need to be placed (i.e. have
    [Let]-expressions made for them) on the upwards traversal. *)
val lifted_constants_still_to_be_placed
   : t
  -> Simplify_envs.Lifted_constant_state.t

val add_lifted_constant_still_to_be_placed
   : t
  -> Simplify_envs.Lifted_constant.t
  -> t

(** Replace the accumulator of lifted constants returned by
    [lifted_constants_still_to_be_placed]. *)
val with_lifted_constants_still_to_be_placed
   : t
  -> Simplify_envs.Lifted_constant_state.t
  -> t

val no_lifted_constants_still_to_be_placed : t -> bool

(** Map the environment component of the given upwards accumulator. *)
val map_uenv
   : t
  -> f:(Simplify_envs.Upwards_env.t
    -> Simplify_envs.Upwards_env.t)
  -> t

(** Replace the environment component of the given upwards accumulator. *)
val with_uenv : t -> Simplify_envs.Upwards_env.t -> t

val remember_code_for_cmx
    : t
  -> Flambda.Function_params_and_body.t Code_id.Map.t
  -> t

val all_code : t -> Exported_code.t

val used_closure_vars : t -> Var_within_closure.Set.t

val shareable_constants : t -> Symbol.t Flambda.Static_const.Map.t
