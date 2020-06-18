(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2019 OCamlPro SAS                                    *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Kinds of Flambda types. *)

(** Empty and known-distinct types. *)
type empty_naked_immediate = private Naked_immediate
type empty_naked_float = private Naked_float
type empty_naked_int32 = private Naked_int32
type empty_naked_int64 = private Naked_int64
type empty_naked_nativeint = private Naked_nativeint

(** GADT indexes. *)
type value = private Value
type naked_immediate = empty_naked_immediate * Target_imm.Set.t
type naked_float = empty_naked_float * Numbers.Float_by_bit_pattern.Set.t
type naked_int32 = empty_naked_int32 * Numbers.Int32.Set.t
type naked_int64 = empty_naked_int64 * Numbers.Int64.Set.t
type naked_nativeint = empty_naked_nativeint * Targetint.Set.t
type fabricated = private Fabricated

module Naked_number_kind : sig
  type t =
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  val print : Format.formatter -> t -> unit
end

(** The kinds themselves. *)
type t = private
  | Value
    (** OCaml values that may exist at source level. *)
  | Naked_number of Naked_number_kind.t
    (** The kind of unboxed numbers and untagged immediates. *)
  | Fabricated
    (** Values which have been introduced by Flambda and are never accessible
        at the source language level (for example sets of closures). *)

type kind = t

(** Constructors for the various kinds. *)
val value : t
val naked_immediate : t
val naked_float : t
val naked_int32 : t
val naked_int64 : t
val naked_nativeint : t
(* CR mshinwell: Fabricated kinds are only used in Flambda_static now.  Make
   a separate type. *)
val fabricated : t

val is_value : t -> bool
val is_naked_float : t -> bool

(** The kind of the unit value. *)
val unit : t

include Identifiable.S with type t := t

module Standard_int : sig
  (** These kinds are known as the "standard integer kinds".  They correspond
      to the usual representations of tagged immediates, 32-bit, 64-bit and
      native integers as expected by the operations in [Flambda_primitive].
      (Boxing of the latter three kinds of integers is handled via explicit
      boxing and unboxing primitives; as such, the boxed versions are not
      known as "standard". *)
  type t =
    | Tagged_immediate
    | Naked_immediate
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  val to_kind : t -> kind

  val print_lowercase : Format.formatter -> t -> unit

  include Identifiable.S with type t := t
end

module Standard_int_or_float : sig
  (** The same as [Standard_int], but also permitting naked floats. *)
  type t =
    | Tagged_immediate
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  val to_kind : t -> kind

  val print_lowercase : Format.formatter -> t -> unit

  include Identifiable.S with type t := t
end

(* CR mshinwell: If the tagging/untagging experiment works, this and various
   other things need renaming, to accommodate untagging. *)
module Boxable_number : sig
  (** These kinds are those of the numbers for which a tailored boxed
      representation exists. *)

  type t =
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint
    | Untagged_immediate

  (** The kind of the _unboxed_ representation of the given [t]. *)
  val to_kind : t -> kind

  val of_naked_number_kind : Naked_number_kind.t -> t

  val print_lowercase : Format.formatter -> t -> unit

  val print_lowercase_short : Format.formatter -> t -> unit

  include Identifiable.S with type t := t
end

(** Witnesses for the naked number kinds, for use when matching on the structure
    of types, to introduce constraints. *)
module Naked_number : sig
  type _ t =
    | Naked_immediate : naked_immediate t
    | Naked_float : naked_float t
    | Naked_int32 : naked_int32 t
    | Naked_int64 : naked_int64 t
    | Naked_nativeint : naked_nativeint t

  val print : Format.formatter -> _ t -> unit
end
