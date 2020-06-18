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

(** Typing environment extensions: typing environment levels that are
    surrounded by a binder that captures defined names as existentials. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val print : Format.formatter -> t -> unit

val invariant : t -> unit

val empty : unit -> t

val is_empty : t -> bool

val create : Typing_env_level.t -> t

val pattern_match : t -> f:(Typing_env_level.t -> 'a) -> 'a

val one_equation : Name.t -> Type_grammar.t -> t

val add_or_replace_equation : t -> Name.t -> Type_grammar.t -> t

val add_cse
   : t
  -> prim:Flambda_primitive.Eligible_for_cse.t
  -> bound_to:Simple.t
  -> t

val meet : Meet_env.t -> t -> t -> t

val n_way_meet : Meet_env.t -> t list -> t

(* val n_way_join
 *    : env_at_fork:Typing_env.t
 *   (\* CR mshinwell: Introduce "continuation use summary" type or somesuch *\)
 *   -> (Typing_env.t * Apply_cont_rewrite_id.t * Continuation_use_kind.t * t) list
 *   -> params:Kinded_parameter.t list
 *   -> t * Continuation_extra_params_and_args.t *)

val join : Meet_or_join_env.t -> params:Kinded_parameter.t list -> t -> t -> t
