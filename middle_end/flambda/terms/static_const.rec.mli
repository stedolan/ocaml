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
  type t = private {
    code_id : Code_id.t;
    params_and_body : Function_params_and_body.t or_deleted;
    newer_version_of : Code_id.t option;
  }
  and 'a or_deleted =
    | Present of 'a
    | Deleted

  val print : Format.formatter -> t -> unit

  val free_names : t -> Name_occurrences.t

  val create
     : Code_id.t  (** needed for [compare], although useful otherwise too *)
    -> params_and_body:Function_params_and_body.t or_deleted
    -> newer_version_of:Code_id.t option
    -> t

  (** A piece of code that is [Deleted] with no [newer_version_of]. *)
  (* CR mshinwell: rename to [create_deleted] *)
  val deleted : Code_id.t -> t

  val code_id : t -> Code_id.t
  val params_and_body : t -> Function_params_and_body.t option

  val make_deleted : t -> t
end

(* CR mshinwell: Somewhere there should be an invariant check that
   code has no free names. *)

(** The static structure of a symbol, possibly with holes, ready to be filled
    with values computed at runtime. *)
type t =
  | Code of Code.t
  | Set_of_closures of Set_of_closures.t
  | Block of Tag.Scannable.t * Mutability.t * (Field_of_block.t list)
  | Boxed_float of Numbers.Float_by_bit_pattern.t Or_variable.t
  | Boxed_int32 of Int32.t Or_variable.t
  | Boxed_int64 of Int64.t Or_variable.t
  | Boxed_nativeint of Targetint.t Or_variable.t
  | Immutable_float_block of Numbers.Float_by_bit_pattern.t Or_variable.t list
  | Immutable_float_array of Numbers.Float_by_bit_pattern.t Or_variable.t list
  | Mutable_string of { initial_value : string; }
  | Immutable_string of string

type static_const = t

include Identifiable.S with type t := t
include Contains_names.S with type t := t
include Contains_ids.S with type t := t

val is_fully_static : t -> bool

val can_share : t -> bool

val must_be_set_of_closures : t -> Set_of_closures.t

val to_code : t -> Code.t option

val match_against_bound_symbols_pattern
   : t
  -> Bound_symbols.Pattern.t
  -> code:(Code_id.t -> Code.t -> 'a)
  -> set_of_closures:(
       closure_symbols:Symbol.t Closure_id.Lmap.t
    -> Set_of_closures.t
    -> 'a)
  -> block_like:(Symbol.t -> t -> 'a)
  -> 'a

(* CR mshinwell: This should probably move to its own file. *)
module Group : sig
  type t

  include Contains_names.S with type t := t
  include Contains_ids.S with type t := t

  val empty : t

  val create : static_const list -> t

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val print : Format.formatter -> t -> unit

  val to_list : t -> static_const list

  val concat : t -> t -> t

  val match_against_bound_symbols
     : t
    -> Bound_symbols.t
    -> init:'a
    -> code:('a -> Code_id.t -> Code.t -> 'a)
    -> set_of_closures:(
        'a
      -> closure_symbols:Symbol.t Closure_id.Lmap.t
      -> Set_of_closures.t
      -> 'a)
    -> block_like:('a -> Symbol.t -> static_const -> 'a)
    -> 'a

  val pieces_of_code : t -> Function_params_and_body.t Code_id.Map.t

  val pieces_of_code' : t -> Code.t list

  val is_fully_static : t -> bool
end
