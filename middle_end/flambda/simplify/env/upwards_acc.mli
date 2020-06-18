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

val create
   : Simplify_env_and_result.Upwards_env.t
  -> Code_age_relation.t
  -> Simplify_env_and_result.Result.t
  -> t

(** Create an upwards accumulator by copying the result structure and
    code age relation out of the given downwards accumulator. *)
val of_dacc : Downwards_acc.t -> t

(** Extract the environment component of the given upwards accumulator. *)
val uenv : t -> Simplify_env_and_result.Upwards_env.t

val code_age_relation : t -> Code_age_relation.t

(** Map the environment component of the given upwards accumulator. *)
val map_uenv
   : t
  -> f:(Simplify_env_and_result.Upwards_env.t
    -> Simplify_env_and_result.Upwards_env.t)
  -> t

(** Map the result structure of the given upwards accumulator. *)
val map_r
   : t
  -> f:(Simplify_env_and_result.Result.t
    -> Simplify_env_and_result.Result.t)
  -> t

(** Replace the environment component of the given upwards accumulator. *)
val with_uenv : t -> Simplify_env_and_result.Upwards_env.t -> t

(** The result structure of the given upwards accumulator. *)
val r : t -> Simplify_env_and_result.Result.t

(** Replace the result structure of the given downwards accumulator. *)
val with_r : t -> Simplify_env_and_result.Result.t -> t
