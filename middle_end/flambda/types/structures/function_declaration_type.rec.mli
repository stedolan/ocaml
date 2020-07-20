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

[@@@ocaml.warning "+a-30-40-41-42"]

module Inlinable : sig
  type t

  val create
     : code_id:Code_id.t
    -> param_arity:Flambda_arity.t
    -> result_arity:Flambda_arity.t
    -> stub:bool
    -> dbg:Debuginfo.t
    -> inline:Inline_attribute.t
    -> is_a_functor:bool
    -> recursive:Recursive.t
    -> rec_info:Rec_info.t
    -> is_tupled:bool
    -> t

  val code_id : t -> Code_id.t
  val param_arity : t -> Flambda_arity.t
  val result_arity : t -> Flambda_arity.t
  val stub : t -> bool
  val dbg : t -> Debuginfo.t
  val inline : t -> Inline_attribute.t
  val is_a_functor : t -> bool
  val recursive : t -> Recursive.t
  val rec_info : t -> Rec_info.t
  val is_tupled : t -> bool
end

module Non_inlinable : sig
  type t

  val create
     : code_id:Code_id.t
    -> param_arity:Flambda_arity.t
    -> result_arity:Flambda_arity.t
    -> recursive:Recursive.t
    -> is_tupled:bool
    -> t

  val code_id : t -> Code_id.t
  val param_arity : t -> Flambda_arity.t
  val result_arity : t -> Flambda_arity.t
  val recursive : t -> Recursive.t
  val is_tupled : t -> bool
end

type t0 =
  | Inlinable of Inlinable.t
  | Non_inlinable of Non_inlinable.t

type t = t0 Or_unknown_or_bottom.t

(* CR mshinwell: Add [create] and make [private]. *)

include Type_structure_intf.S
  with type t := t
  with type flambda_type := Type_grammar.t
  with type typing_env := Typing_env.t
  with type meet_env := Meet_env.t
  with type meet_or_join_env := Meet_or_join_env.t
  with type typing_env_extension := Typing_env_extension.t

val apply_rec_info : t -> Rec_info.t -> t Or_bottom.t
