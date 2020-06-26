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

(** The interface to the Flambda type system.  This is parameterised over
    the expression language via [Code_id]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: Add invariant checks, including e.g. on the bodies of
   functions in types. *)

module Float = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

type t
type flambda_type = t

val print : Format.formatter -> t -> unit

val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

val invariant : Invariant_env.t -> t -> unit

val arity_of_list : t list -> Flambda_arity.t

type typing_env
type typing_env_extension

module Typing_env_extension : sig
  type t = typing_env_extension

  val print : Format.formatter -> t -> unit

  val invariant : t -> unit

  val empty : unit -> t

  val one_equation : Name.t -> flambda_type -> t

  val add_or_replace_equation : t -> Name.t -> flambda_type -> t

  val add_cse
     : t
    -> prim:Flambda_primitive.Eligible_for_cse.t
    -> bound_to:Simple.t
    -> t
end

module Typing_env : sig
  type t = typing_env

  val invariant : t -> unit

  val print : Format.formatter -> t -> unit

  val create
     : resolver:(Compilation_unit.t -> t option)
    -> get_imported_names:(unit -> Name.Set.t)
    -> t

  val closure_env : t -> t

  val resolver : t -> (Compilation_unit.t -> t option)

  val code_age_relation_resolver
     : t
    -> (Compilation_unit.t -> Code_age_relation.t option)

  val name_domain : t -> Name.Set.t

  val current_scope : t -> Scope.t

  val increment_scope : t -> t

  val add_definition : t -> Name_in_binding_pos.t -> Flambda_kind.t -> t

  val add_definitions_of_params : t -> params:Kinded_parameter.t list -> t

  val add_symbol_definition : t -> Symbol.t -> t

  val add_equation : t -> Name.t -> flambda_type -> t

  val add_equations_on_params
     : t
    -> params:Kinded_parameter.t list
    -> param_types:flambda_type list
    -> t

  val add_cse
     : t
    -> Flambda_primitive.Eligible_for_cse.t
    -> bound_to:Simple.t
    -> t

  val mem : ?min_name_mode:Name_mode.t -> t -> Name.t -> bool

  val mem_simple : ?min_name_mode:Name_mode.t -> t -> Simple.t -> bool

  val find : t -> Name.t -> Flambda_kind.t option -> flambda_type

  val find_params : t -> Kinded_parameter.t list -> flambda_type list

  val find_cse : t -> Flambda_primitive.Eligible_for_cse.t -> Simple.t option

  val add_env_extension : t -> Typing_env_extension.t -> t

  (** Raises [Not_found] if no canonical [Simple] was found. *)
  val get_canonical_simple_exn
     : t
    -> ?min_name_mode:Name_mode.t
    -> Simple.t
    -> Simple.t

  (** Raises [Not_found] if no canonical [Simple] was found. *)
  val get_canonical_simple_with_kind_exn
     : t
    -> ?min_name_mode:Name_mode.t
    -> Simple.t
    -> Simple.t * Flambda_kind.t

  val add_to_code_age_relation : t -> newer:Code_id.t -> older:Code_id.t -> t

  val code_age_relation : t -> Code_age_relation.t

  val with_code_age_relation : t -> Code_age_relation.t -> t

  val cut_and_n_way_join
     : t
    -> (t * Apply_cont_rewrite_id.t * Continuation_use_kind.t) list
    -> params:Kinded_parameter.t list
    -> unknown_if_defined_at_or_later_than:Scope.t
    -> Typing_env_extension.t * Continuation_extra_params_and_args.t

  val free_names_transitive
     : t
    -> flambda_type
    -> Name_occurrences.t

  val clean_for_export : t -> module_symbol:Symbol.t -> t

  module Serializable : sig
    type typing_env = t
    type t

    val create : typing_env -> t

    val print : Format.formatter -> t -> unit

    val to_typing_env
       : t
      -> resolver:(Compilation_unit.t -> typing_env option)
      -> get_imported_names:(unit -> Name.Set.t)
      -> typing_env

    val all_ids_for_export : t -> Ids_for_export.t

    val import : Ids_for_export.Import_map.t -> t -> t

    val merge : t -> t -> t
  end
end

val meet : Typing_env.t -> t -> t -> (t * Typing_env_extension.t) Or_bottom.t

val meet_shape
   : Typing_env.t
  -> t
  -> shape:t
  -> result_var:Var_in_binding_pos.t
  -> result_kind:Flambda_kind.t
  -> Typing_env_extension.t Or_bottom.t

val join
   : ?bound_name:Name.t
  -> Typing_env.t
  -> left_env:Typing_env.t
  -> left_ty:t
  -> right_env:Typing_env.t
  -> right_ty:t
  -> t

(* CR mshinwell: Substitute out this alias once it's finalised *)
type 'a type_accessor = Typing_env.t -> 'a

module Function_declaration_type : sig
  module Inlinable : sig
    type t

    val code_id : t -> Code_id.t
    val param_arity : t -> Flambda_arity.t
    val result_arity : t -> Flambda_arity.t
    val stub : t -> bool
    val dbg : t -> Debuginfo.t
    val inline : t -> Inline_attribute.t
    val is_a_functor : t -> bool
    val recursive : t -> Recursive.t
    val rec_info : t -> Rec_info.t
  end

  module Non_inlinable : sig
    type t

    val code_id : t -> Code_id.t
    val param_arity : t -> Flambda_arity.t
    val result_arity : t -> Flambda_arity.t
    val recursive : t -> Recursive.t
  end

  type t0 = private
    | Inlinable of Inlinable.t
    | Non_inlinable of Non_inlinable.t

  type t = t0 Or_unknown_or_bottom.t
end

module Closures_entry : sig
  type t

  val closure_types : t -> flambda_type Closure_id.Map.t
  val function_decl_types : t -> Function_declaration_type.t Closure_id.Map.t
  val closure_var_types : t -> flambda_type Var_within_closure.Map.t
end

(** This function takes a type [t] and an environment [env] that assigns types
    to all the free names of [t].  It also takes an environment, called
    [suitable_for], in which we would like to use [t].  The function
    identifies which free names (if any) of [t] would be unbound in
    [suitable_for].  For each such name a fresh variable is assigned and
    irrelevantly bound in [suitable_for]; the returned type is like [t]
    except that the names that would otherwise be unbound are replaced by
    these fresh variables.  The fresh variables are assigned types in the
    returned environment extension on a best effort basis. *)
(* CR mshinwell: update comment *)
val make_suitable_for_environment
   : t
  -> Typing_env.t
  -> suitable_for:Typing_env.t
  -> bind_to:Name.t
  -> Typing_env_extension.t

val apply_rec_info : flambda_type -> Rec_info.t -> flambda_type Or_bottom.t

(** Construct a bottom type of the given kind. *)
val bottom : Flambda_kind.t -> t

(** Construct a top ("unknown") type of the given kind. *)
val unknown : Flambda_kind.t -> t

(** Create an bottom type with the same kind as the given type. *)
val bottom_like : t -> t

(** Create an "unknown" type with the same kind as the given type. *)
val unknown_like : t -> t

val any_value : unit -> t

val any_tagged_immediate : unit -> t
val any_tagged_bool : unit -> t

val any_boxed_float : unit -> t
val any_boxed_int32 : unit -> t
val any_boxed_int64 : unit -> t
val any_boxed_nativeint : unit -> t

val any_naked_immediate : unit -> t
val any_naked_float : unit -> t
val any_naked_int32 : unit -> t
val any_naked_int64 : unit -> t
val any_naked_nativeint : unit -> t

(** Building of types representing tagged / boxed values from specified
    constants. *)
val this_tagged_immediate : Target_imm.t -> t
val this_boxed_float : Numbers.Float_by_bit_pattern.t -> t
val this_boxed_int32 : Int32.t -> t
val this_boxed_int64 : Int64.t -> t
val this_boxed_nativeint : Targetint.t -> t

val these_tagged_immediates : Target_imm.Set.t -> t
val these_boxed_floats : Numbers.Float_by_bit_pattern.Set.t -> t
val these_boxed_int32s : Int32.Set.t -> t
val these_boxed_int64s : Int64.Set.t -> t
val these_boxed_nativeints : Targetint.Set.t -> t

(** Building of types representing untagged / unboxed values from
    specified constants. *)
val this_naked_immediate : Target_imm.t -> t
val this_naked_float : Numbers.Float_by_bit_pattern.t -> t
val this_naked_int32 : Int32.t -> t
val this_naked_int64 : Int64.t -> t
val this_naked_nativeint : Targetint.t -> t

val these_naked_immediates : Target_imm.Set.t -> t
val these_naked_floats : Numbers.Float_by_bit_pattern.Set.t -> t
val these_naked_int32s : Int32.Set.t -> t
val these_naked_int64s : Int64.Set.t -> t
val these_naked_nativeints : Targetint.Set.t -> t

val boxed_float_alias_to : naked_float:Variable.t -> t
val boxed_int32_alias_to : naked_int32:Variable.t -> t
val boxed_int64_alias_to : naked_int64:Variable.t -> t
val boxed_nativeint_alias_to : naked_nativeint:Variable.t -> t

val box_float : t -> t
val box_int32 : t -> t
val box_int64 : t -> t
val box_nativeint : t -> t

val tagged_immediate_alias_to : naked_immediate:Variable.t -> t
val tag_immediate : t -> t

val is_int_for_scrutinee : scrutinee:Simple.t -> t
val get_tag_for_block : block:Simple.t -> t

(* CR mshinwell: decide on exact strategy for mutable blocks *)

(** The type of an immutable block with a known tag, size and field types. *)
val immutable_block
   : is_unique:bool
  -> Tag.t
  -> field_kind:Flambda_kind.t
  -> fields:t list
  -> t

(** The type of an immutable block with at least [n] fields and an unknown
    tag. The type of the [n - 1]th field is taken to be an [Equals] to the
    given variable. *)
val immutable_block_with_size_at_least
   : tag:Tag.t Or_unknown.t
  -> n:Targetint.OCaml.t
  -> field_kind:Flambda_kind.t
  -> field_n_minus_one:Variable.t
  -> t

val variant
   : const_ctors:t
  -> non_const_ctors:t list Tag.Scannable.Map.t
  -> t

val open_variant_from_const_ctors_type : const_ctors:t -> t

val open_variant_from_non_const_ctor_with_size_at_least
   : n:Targetint.OCaml.t
  -> field_n_minus_one:Variable.t
  -> t

val this_immutable_string : string -> t

val mutable_string : size:int -> t

(** Create a description of a function declaration whose code is known. *)
val create_inlinable_function_declaration
   : code_id:Code_id.t
  -> param_arity:Flambda_arity.t
  -> result_arity:Flambda_arity.t
  -> stub:bool
  -> dbg:Debuginfo.t
  -> inline:Inline_attribute.t
  -> is_a_functor:bool
  -> recursive:Recursive.t
  -> rec_info:Rec_info.t
  -> Function_declaration_type.t

(** Create a description of a function declaration whose code is unknown.
    Such declarations cannot be inlined, but can be direct called. *)
val create_non_inlinable_function_declaration
   : code_id:Code_id.t
  -> param_arity:Flambda_arity.t
  -> result_arity:Flambda_arity.t
  -> recursive:Recursive.t
  -> Function_declaration_type.t

val exactly_this_closure
   : Closure_id.t
  -> all_function_decls_in_set:Function_declaration_type.t Closure_id.Map.t
  -> all_closures_in_set:t Closure_id.Map.t
  -> all_closure_vars_in_set:flambda_type Var_within_closure.Map.t
  -> flambda_type

val at_least_the_closures_with_ids
   : this_closure:Closure_id.t
  -> Simple.t Closure_id.Map.t
  -> flambda_type

val closure_with_at_least_this_closure_var
   : this_closure:Closure_id.t
  -> Var_within_closure.t
  -> closure_element_var:Variable.t
  -> flambda_type

val array_of_length : length:flambda_type -> flambda_type

(** Construct a type equal to the type of the given name.  (The name
    must be present in the given environment when calling e.g. [join].) *)
val alias_type_of : Flambda_kind.t -> Simple.t -> t

(** Determine the (unique) kind of a type. *)
val kind : t -> Flambda_kind.t

val get_alias_exn : t -> Simple.t

(** For each of the kinds in an arity, create an "unknown" type. *)
val unknown_types_from_arity : Flambda_arity.t -> t list

(** For each of the kinds in an arity, create an "bottom" type. *)
val bottom_types_from_arity : Flambda_arity.t -> t list

(** Whether the given type says that a term of that type can never be
    constructed (in other words, it is [Invalid]). *)
val is_bottom : (t -> bool) type_accessor

val type_for_const : Reg_width_const.t -> t
val kind_for_const : Reg_width_const.t -> Flambda_kind.t

type 'a proof = private
  | Proved of 'a
  | Unknown
  | Invalid

type 'a proof_allowing_kind_mismatch = private
  | Proved of 'a
  | Unknown
  | Invalid
  | Wrong_kind

(* CR mshinwell: Should remove "_equals_" from these names *)
val prove_equals_tagged_immediates
   : Typing_env.t
  -> t
  -> Target_imm.Set.t proof

val prove_naked_immediates
   : Typing_env.t
  -> t
  -> Target_imm.Set.t proof

val prove_equals_single_tagged_immediate
   : Typing_env.t
  -> t
  -> Target_imm.t proof

val prove_naked_floats
   : Typing_env.t
  -> t
  -> Numbers.Float_by_bit_pattern.Set.t proof

val prove_naked_int32s : Typing_env.t -> t -> Numbers.Int32.Set.t proof

val prove_naked_int64s : Typing_env.t -> t -> Numbers.Int64.Set.t proof

val prove_naked_nativeints : Typing_env.t -> t -> Targetint.Set.t proof

type variant_proof = private {
  const_ctors : Target_imm.Set.t;
  non_const_ctors_with_sizes : Targetint.OCaml.t Tag.Scannable.Map.t;
}

val prove_variant
   : Typing_env.t
  -> t
  -> variant_proof proof_allowing_kind_mismatch

val prove_is_a_tagged_immediate
   : Typing_env.t
  -> t
  -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_float
   : Typing_env.t
  -> t
  -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_int32
   : Typing_env.t
  -> t
  -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_int64
   : Typing_env.t
  -> t
  -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_nativeint
   : Typing_env.t
  -> t
  -> unit proof_allowing_kind_mismatch

val prove_boxed_floats : Typing_env.t -> t -> Float.Set.t proof
val prove_boxed_int32s : Typing_env.t -> t -> Numbers.Int32.Set.t proof
val prove_boxed_int64s : Typing_env.t -> t -> Numbers.Int64.Set.t proof
val prove_boxed_nativeints : Typing_env.t -> t -> Targetint.Set.t proof

val prove_tags_and_sizes
   : Typing_env.t
  -> t
  -> Targetint.OCaml.t Tag.Map.t proof

val prove_unique_tag_and_size
   : Typing_env.t
  -> t
  -> (Tag.t * Targetint.OCaml.t) proof_allowing_kind_mismatch

val prove_is_int : Typing_env.t -> t -> bool proof

(** Prove that the given type, of kind [Value], is a closures type describing
    exactly one set of closures. The function declaration type corresponding to
    such closure is returned together with its closure ID, if it is known. *)
(* CR mshinwell: Fix comment and/or function name *)
val prove_single_closures_entry
   : Typing_env.t
  -> t
  -> (Closure_id.t * Closures_entry.t * Function_declaration_type.t) proof

val prove_single_closures_entry'
   : Typing_env.t
  -> t
  -> (Closure_id.t * Closures_entry.t * Function_declaration_type.t)
       proof_allowing_kind_mismatch

val prove_strings : Typing_env.t -> t -> String_info.Set.t proof

type var_or_symbol_or_tagged_immediate = private
  | Var of Variable.t
  | Symbol of Symbol.t
  | Tagged_immediate of Target_imm.t

type to_lift = (* private *) (* CR mshinwell: resurrect *)
  | Immutable_block of
      { tag : Tag.Scannable.t;
        is_unique : bool;
        fields : (var_or_symbol_or_tagged_immediate list);
      }
  | Boxed_float of Float.t
  | Boxed_int32 of Int32.t
  | Boxed_int64 of Int64.t
  | Boxed_nativeint of Targetint.t

type reification_result = private
  | Lift of to_lift  (* CR mshinwell: rename? *)
  | Lift_set_of_closures of {
      closure_id : Closure_id.t;
      function_decls : Function_declaration_type.Inlinable.t Closure_id.Map.t;
      closure_vars : Simple.t Var_within_closure.Map.t;
    }
  | Simple of Simple.t
  | Cannot_reify
  | Invalid

val reify
   : ?allowed_if_free_vars_defined_in:Typing_env.t
  -> ?disallowed_free_vars:Variable.Set.t
  -> ?allow_unique:bool
  -> Typing_env.t
  -> min_name_mode:Name_mode.t
  -> t
  -> reification_result
