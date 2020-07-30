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

(** Print a downwards accumulator to a formatter. *)
val print : Format.formatter -> t -> unit

(** Create a downwards accumulator. *)
val create
   : Simplify_envs.Downwards_env.t
  -> Continuation_uses_env.t
  -> t

(** Extract the environment component of the given downwards accumulator. *)
val denv : t -> Simplify_envs.Downwards_env.t

(** Map the environment component of the given downwards accumulator. *)
val map_denv
   : t
  -> f:(Simplify_envs.Downwards_env.t
    -> Simplify_envs.Downwards_env.t)
  -> t

val map_denv2
   : t
  -> f:(Simplify_envs.Downwards_env.t
    -> Simplify_envs.Downwards_env.t * 'a)
  -> t * 'a

(** Replace the environment component of the given downwards accumulator. *)
val with_denv : t -> Simplify_envs.Downwards_env.t -> t

(* CR mshinwell: Why do these take scope arguments when [DE] knows the
   current scope level? *)
include Continuation_uses_env_intf.S with type t := t

val continuation_uses_env : t -> Continuation_uses_env.t

val with_continuation_uses_env : t -> cont_uses_env:Continuation_uses_env.t -> t

val code_age_relation : t -> Code_age_relation.t

val with_code_age_relation : t -> Code_age_relation.t -> t

val typing_env : t -> Flambda_type.Typing_env.t

val add_variable : t -> Var_in_binding_pos.t -> Flambda_type.t -> t

val extend_typing_environment : t -> Flambda_type.Typing_env_extension.t -> t

val no_lifted_constants : t -> bool

val add_lifted_constant
   : t
  -> Simplify_envs.Lifted_constant.t
  -> t

val add_lifted_constants_from_list
   : t
  -> Simplify_envs.Lifted_constant.t list
  -> t

val add_lifted_constants
   : t
  -> Simplify_envs.Lifted_constant_state.t -> t

val get_lifted_constants
   : t
  -> Simplify_envs.Lifted_constant_state.t

val get_and_clear_lifted_constants
   : t
  -> t * Simplify_envs.Lifted_constant_state.t

val clear_lifted_constants : t -> t

val set_lifted_constants
   : t
  -> Simplify_envs.Lifted_constant_state.t
  -> t

val find_shareable_constant : t -> Flambda.Static_const.t -> Symbol.t option

val consider_constant_for_sharing : t -> Symbol.t -> Flambda.Static_const.t -> t

val with_shareable_constants
   : t
  -> shareable_constants:Symbol.t Flambda.Static_const.Map.t
  -> t

val shareable_constants : t -> Symbol.t Flambda.Static_const.Map.t

val add_use_of_closure_var : t -> Var_within_closure.t -> t

val used_closure_vars : t -> Var_within_closure.Set.t

val with_used_closure_vars
   : t
  -> used_closure_vars:Var_within_closure.Set.t
  -> t
