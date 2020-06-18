(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Effects and coeffects *)

type t = Effects.t * Coeffects.t
(** A pair of an effect and a coeffect. *)

val print : Format.formatter -> t -> unit
(** Print *)

val compare : t -> t -> int
(** Comparison. *)

val pure : t
(** The value stating that no effects of coeffects take place.
    This is exactly [No_effects, No_coeffects]. *)

val all : t
(** The value stating that any effects and/or coeffects may take
    place. This is exactly [Arbitrary_effects, Has_coeffects]. *)

val read : t
(** The calue stating that a read (i.e only a coeffect) takes place.
    This is [No_effects, Has_coeffects]. *)

val is_pure : t -> bool
(** Is the expression with the given effects and coeffects pure ?
    In other words, can it commute with any expression ? *)

val commute : t -> t -> bool
(** Can two expressions with the given effects and coeffects be commuted
    without changing their semantics. *)

val has_commuting_effects : t -> bool
(** Does the given effects and coeffects has observable effects ?
    Rather, does it have some effects with regards to whether it can
    commute with other expressions. *)

val has_commuting_coeffects : t -> bool
(** Does the given effects and coeffects has observable coeffects ?
    Rather, does it have some coeffects with regards to whether it can
    commute with other expressions. *)

val join : t -> t -> t
(** Join two effects and coeffects. *)

