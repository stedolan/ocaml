(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Target_imm constants that can be held in registers. *)

type 'a or_wrong = private
  | Ok of 'a
  | Wrong

type t = private {
  value : Targetint.OCaml.t;
  print_as_char : bool;
}

type immediate = t

(** The comparison function for type [t] ignores [print_as_char]. *)
include Identifiable.S with type t := t

val one : t
val zero : t
val minus_one : t

val join : t -> t -> t or_wrong

val join_set : Set.t -> Set.t -> Set.t

val bool_true : t
val bool_false : t

val bool : bool -> t

val int : Targetint.OCaml.t -> t
val char : char -> t

val tag : Tag.t -> t
val to_tag : t -> Tag.t option

val map : t -> f:(Targetint.OCaml.t -> Targetint.OCaml.t) -> t

val is_non_negative : t -> bool

(* CR mshinwell: bad names *)
val to_targetint : t -> Targetint.OCaml.t
val to_targetint' : t -> Targetint.t

val set_to_targetint_set : Set.t -> Targetint.OCaml.Set.t
val set_to_targetint_set' : Set.t -> Targetint.Set.t

val set_of_targetint_set : Targetint.OCaml.Set.t -> Set.t
val set_of_targetint_set' : Targetint.Set.t -> Set.t

val neg : t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val mod_ : t -> t -> t
val and_ : t -> t -> t
val or_ : t -> t -> t
val xor : t -> t -> t
val shift_left : t -> int -> t
val shift_right : t -> int -> t
val shift_right_logical : t -> int -> t

val get_least_significant_16_bits_then_byte_swap : t -> t

(** The set consisting of the representations of constant [true] and [false]. *)
val all_bools : Set.t

module Or_unknown : sig
  type nonrec t = private
    | Ok of t
    | Unknown

  val ok : immediate -> t
  val unknown : unit -> t

  include Identifiable.S with type t := t
end

module Pair : sig
  type nonrec t = t * t

  include Identifiable.S with type t := t
end

val cross_product : Set.t -> Set.t -> Pair.Set.t
