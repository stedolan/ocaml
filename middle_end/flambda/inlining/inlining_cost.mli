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

(** Measurement of the cost (including cost in space) of Flambda terms
    in the context of inlining. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

module Threshold : sig
  (** The maximum size, in some abstract measure of space cost, that an
      Flambda expression may be in order to be inlined. *)
  type t =
    | Never_inline
    | Can_inline_if_no_larger_than of int

  val add : t -> t -> t
  val sub : t -> t -> t
  val min : t -> t -> t
end

(* Determine whether the given Flambda expression has a sufficiently low space
   cost so as to fit under the given [inlining_threshold].  The [bonus] is
   added to the threshold before evaluation. *)
val can_inline
   : Simplify_envs.Downwards_env.t
  -> Expr.t
  -> Threshold.t
  -> bonus:int
  -> bool

module Benefit : sig
  (* A model of the benefit we gain by removing a particular combination
     of operations.  Such removals are typically performed by inlining (for
     example, [remove_call]) and simplification (for example, [remove_alloc])
     passes. *)

  type t

  val zero : t
  val (+) : t -> t -> t
  val max : round:int -> t -> t -> t

  val remove_call : t -> t
  (* CR mshinwell: can we use remove_primitive instead of remove_alloc etc? *)
  (* CR-soon mshinwell: [remove_alloc] should take the size of the block
     (to account for removal of initializing writes). *)
  val remove_alloc : t -> t
  val add_primitive : Flambda_primitive.Without_args.t -> t -> t
  val remove_primitive : Flambda_primitive.Without_args.t -> t -> t
  val remove_primitive_application : Flambda_primitive.t -> t -> t
  val remove_branch : t -> t
  val direct_call_of_indirect_unknown_arity : t -> t
  val direct_call_of_indirect_known_arity : t -> t
  val requested_inline
     : Simplify_envs.Downwards_env.t
    -> t
    -> size_of:Expr.t
    -> t

  val print : Format.formatter -> t -> unit
end

val scale_inline_threshold_by : int
