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

(** Language terms that represent statically-allocated values. *)

module Field_of_block : sig
  (** Inhabitants (of kind [Value]) of fields of statically-allocated blocks. *)
  type t =
    | Symbol of Symbol.t
      (** The address of the given symbol. *)
    | Tagged_immediate of Target_imm.t
      (** The given tagged immediate. *)
    | Dynamically_computed of Variable.t
      (** The value of the given variable. *)

  (** Printing, total ordering, etc. *)
  include Identifiable.S with type t := t
end

(** A piece of code, comprising of the parameters and body of a function,
    together with a field indicating whether the piece of code is a newer
    version of one that existed previously (and may still exist), for
    example after a round of simplification. *)
module Code : sig
  type t = {
    params_and_body : Function_params_and_body.t or_deleted;
    newer_version_of : Code_id.t option;
  }
  and 'a or_deleted =
    | Present of 'a
    | Deleted

  val print : Format.formatter -> t -> unit

  val free_names : t -> Name_occurrences.t

  val make_deleted : t -> t
end

(* CR mshinwell: Somewhere there should be an invariant check that
   code has no free names. *)

(** The possibly-recursive declaration of pieces of code and any associated set
    of closures. *)
module Code_and_set_of_closures : sig
  type t = {
    code : Code.t Code_id.Map.t;
    (* CR mshinwell: Check the free names of the set of closures *)
    set_of_closures : Set_of_closures.t;
  }

  val map_code : t -> f:(Code_id.t -> Code.t -> Code.t) -> t
end

(** The static structure of a symbol, possibly with holes, ready to be filled
    with values computed at runtime. *)
type t =
  | Block of Tag.Scannable.t * Mutable_or_immutable.t * (Field_of_block.t list)
  | Sets_of_closures of Code_and_set_of_closures.t list
    (** All code and sets of closures within the list are allowed to be
        recursive across those sets (but not recursive with any other code or
        set of closures). *)
  | Boxed_float of Numbers.Float_by_bit_pattern.t Or_variable.t
  | Boxed_int32 of Int32.t Or_variable.t
  | Boxed_int64 of Int64.t Or_variable.t
  | Boxed_nativeint of Targetint.t Or_variable.t
  | Immutable_float_block of Numbers.Float_by_bit_pattern.t Or_variable.t list
  | Immutable_float_array of Numbers.Float_by_bit_pattern.t Or_variable.t list
  | Mutable_string of { initial_value : string; }
  | Immutable_string of string

include Identifiable.S with type t := t
include Contains_names.S with type t := t
include Contains_ids.S with type t := t

val get_pieces_of_code : t -> Code.t Code_id.Map.t

val get_pieces_of_code' : t -> Function_params_and_body.t Code_id.Map.t

val is_fully_static : t -> bool

val can_share : t -> bool

val must_be_sets_of_closures : t -> Code_and_set_of_closures.t list
