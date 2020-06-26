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

(** The defining expressions of [Let] bindings. *)
type t = private
  | Simple of Simple.t
    (** Things that fit in a register (variables, symbols, constants).
        These do not have to be [Let]-bound but are allowed here for
        convenience. *)
  | Prim of Flambda_primitive.t * Debuginfo.t
    (** Primitive operations (arithmetic, memory access, allocation, etc). *)
  | Set_of_closures of Set_of_closures.t
    (** Definition of a set of possibly mutually-recursive closures. *)

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

include Contains_ids.S with type t := t

(** Convert a register-width value into the defining expression of a [Let]. *)
val create_simple : Simple.t -> t

(** Convert a primitive, with associated debugging information, into the
    defining expression of a [Let]. *)
val create_prim : Flambda_primitive.t -> Debuginfo.t -> t

(** Convert a set of closures into the defining expression of a [Let]. *)
val create_set_of_closures : Set_of_closures.t -> t

(** Build an expression boxing the name.  The returned kind is the
    one of the unboxed version. *)
val box_value
  : Name.t
 -> Flambda_kind.t
 -> Debuginfo.t
 -> Named.t * Flambda_kind.t

(** Build an expression unboxing the name.  The returned kind is the
    one of the unboxed version. *)
val unbox_value
  : Name.t
 -> Flambda_kind.t
 -> Debuginfo.t
 -> Named.t * Flambda_kind.t

(** Return a defining expression for a [Let] which is kind-correct, but not
    necessarily type-correct, at the given kind. *)
val dummy_value : Flambda_kind.t -> t

val at_most_generative_effects : t -> bool

val invariant_returning_kind
   : Invariant_env.t
  -> t
  -> Flambda_primitive.result_kind

val is_set_of_closures : t -> bool
