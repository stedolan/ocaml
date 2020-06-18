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

open! Flambda.Import

type resolver = Compilation_unit.t -> Flambda_type.Typing_env.t option
type get_imported_names = unit -> Name.Set.t
type get_imported_code =
  unit -> Function_params_and_body.t Code_id.Map.t

module type Downwards_env = sig
  type t

  type result
  type lifted_constant

  val invariant : t -> unit

  (** Print a human-readable version of the given environment. *)
  val print : Format.formatter -> t -> unit

  (** Create a new environment, marked as being at the toplevel of a
      compilation unit. *)
  val create
     : round:int
    -> backend:(module Flambda_backend_intf.S)
    -> resolver:resolver
    -> get_imported_names:get_imported_names
    -> get_imported_code:get_imported_code
    -> float_const_prop:bool
    -> unit_toplevel_exn_continuation:Continuation.t
    -> t

  (** Obtain the first-class module that gives information about the
      compiler backend being used for compilation. *)
  val backend : t -> (module Flambda_backend_intf.S)

  val resolver : t -> (Compilation_unit.t -> Flambda_type.Typing_env.t option)

  val float_const_prop : t -> bool

  val at_unit_toplevel : t -> bool

  val set_not_at_unit_toplevel : t -> t

  val unit_toplevel_exn_continuation : t -> Continuation.t

  val enter_closure : t -> t

  val increment_continuation_scope_level : t -> t

  val increment_continuation_scope_level_twice : t -> t

  val get_continuation_scope_level : t -> Scope.t

  val now_defining_symbol : t -> Symbol.t -> t

  val no_longer_defining_symbol : t -> Symbol.t -> t

  val symbol_is_currently_being_defined : t -> Symbol.t -> bool

  val symbols_currently_being_defined : t -> Symbol.Set.t

  val typing_env : t -> Flambda_type.Typing_env.t

  val define_variable : t -> Var_in_binding_pos.t -> Flambda_kind.t -> t

  val add_name : t -> Name_in_binding_pos.t -> Flambda_type.t -> t

  val add_variable : t -> Var_in_binding_pos.t -> Flambda_type.t -> t

  val add_equation_on_variable : t -> Variable.t -> Flambda_type.t -> t

  val find_variable : t -> Variable.t -> Flambda_type.t

  val mem_variable : t -> Variable.t -> bool

  val add_symbol : t -> Symbol.t -> Flambda_type.t -> t

  val define_symbol : t -> Symbol.t -> Flambda_kind.t -> t

  val define_symbol_if_undefined : t -> Symbol.t -> Flambda_kind.t -> t

  val mem_symbol : t -> Symbol.t -> bool

  val find_symbol : t -> Symbol.t -> Flambda_type.t

  val add_equation_on_symbol : t -> Symbol.t -> Flambda_type.t -> t

  val define_name : t -> Name_in_binding_pos.t -> Flambda_kind.t -> t

  val define_name_if_undefined
     : t
    -> Name_in_binding_pos.t
    -> Flambda_kind.t
    -> t

  val add_equation_on_name : t -> Name.t -> Flambda_type.t -> t

  val define_parameters : t -> params:Kinded_parameter.t list -> t

  val define_parameters_as_bottom : t -> params:Kinded_parameter.t list -> t

  val add_parameters
     : t
    -> Kinded_parameter.t list
    -> param_types:Flambda_type.t list
    -> t

  val add_parameters_with_unknown_types : t -> Kinded_parameter.t list -> t

  val extend_typing_environment : t -> Flambda_type.Typing_env_extension.t -> t

  val with_typing_env : t -> Flambda_type.Typing_env.t -> t

  val map_typing_env
     : t
    -> f:(Flambda_type.Typing_env.t -> Flambda_type.Typing_env.t)
    -> t

  val check_variable_is_bound : t -> Variable.t -> unit

  val check_symbol_is_bound : t -> Symbol.t -> unit

  val check_name_is_bound : t -> Name.t -> unit

  val check_simple_is_bound : t -> Simple.t -> unit

  val check_code_id_is_bound : t -> Code_id.t -> unit

  val add_lifted_constants : t -> lifted:lifted_constant list -> t

  val define_code
     : t
    -> ?newer_version_of:Code_id.t
    -> code_id:Code_id.t
    -> params_and_body:Function_params_and_body.t
    -> t

  val mem_code : t -> Code_id.t -> bool

  val find_code : t -> Code_id.t -> Function_params_and_body.t

  val with_code : from:t -> t -> t

  (** Appends the locations of inlined call-sites to the given debuginfo
      and sets the resulting debuginfo as the current one in the
      environment. *)
  val add_inlined_debuginfo : t -> Debuginfo.t -> t (* CR mshinwell: remove? *)

  val set_inlined_debuginfo : t -> Debuginfo.t -> t

  val add_inlined_debuginfo' : t -> Debuginfo.t -> Debuginfo.t

  val round : t -> int

  (** Prevent function inlining from occurring in the given environment. *)
  val disable_function_inlining : t -> t

  val can_inline : t -> bool

  val set_inlining_depth_increment : t -> int -> t

  val get_inlining_depth_increment : t -> int
end

module type Upwards_env = sig
  type t

  type downwards_env

  val empty : t

  val invariant : t -> unit

  val print : Format.formatter -> t -> unit

  val add_continuation
     : t
    -> Continuation.t
    -> Scope.t
    -> Flambda_arity.t
    -> t

  val add_continuation_with_handler
     : t
    -> Continuation.t
    -> Scope.t
    -> Flambda_arity.t
    -> Continuation_handler.t
    -> t

  val add_unreachable_continuation
     : t
    -> Continuation.t
    -> Scope.t
    -> Flambda_arity.t
    -> t

  val add_continuation_alias
     : t
    -> Continuation.t
    -> Flambda_arity.t
    -> alias_for:Continuation.t
    -> t

  val add_continuation_to_inline
     : t
    -> Continuation.t
    -> Scope.t
    -> Flambda_arity.t
    -> Continuation_handler.t
    -> t

  val add_exn_continuation
     : t
    -> Exn_continuation.t
    -> Scope.t
    -> t

  val find_continuation : t -> Continuation.t -> Continuation_in_env.t

  val mem_continuation : t -> Continuation.t -> bool

  val resolve_continuation_aliases : t -> Continuation.t -> Continuation.t

  val resolve_exn_continuation_aliases
     : t
    -> Exn_continuation.t
    -> Exn_continuation.t

  val continuation_arity : t -> Continuation.t -> Flambda_arity.t

  val check_continuation_is_bound : t -> Continuation.t -> unit

  val check_exn_continuation_is_bound : t -> Exn_continuation.t -> unit

  val add_apply_cont_rewrite
     : t
    -> Continuation.t
    -> Apply_cont_rewrite.t
    -> t

  val find_apply_cont_rewrite
     : t
    -> Continuation.t
    -> Apply_cont_rewrite.t option
end

module type Result = sig
  type lifted_constant

  type t

  val print : Format.formatter -> t -> unit

  val create
     : resolver:resolver
    -> get_imported_names:get_imported_names
    -> t

  val new_lifted_constant : t -> lifted_constant -> t

(* can cause duplicates
  val add_lifted_constants : t -> from:t -> t
*)

  (** Retrieve constants lifted to toplevel.  The constants must be defined
      in the order returned (first element of the list defined first). *)
  (* CR mshinwell: Update name to reflect this *)
  val get_lifted_constants : t -> lifted_constant list

  val get_and_clear_lifted_constants : t -> t * (lifted_constant list)

  val add_prior_lifted_constants : t -> lifted_constant list -> t

  val set_lifted_constants : t -> lifted_constant list -> t

  (* CR mshinwell: This whole lifted/shareable constant interface probably
     still needs some work *)
  val find_shareable_constant : t -> Static_const.t -> Symbol.t option

  val consider_constant_for_sharing : t -> Symbol.t -> Static_const.t -> t

  val remember_code_for_cmx
     : t
    -> Flambda.Function_params_and_body.t Code_id.Map.t
    -> t

  val all_code : t -> Flambda.Function_params_and_body.t Code_id.Map.t

  val clear_lifted_constants : t -> t

  val add_use_of_closure_var : t -> Var_within_closure.t -> t

  val used_closure_vars : t -> Var_within_closure.Set.t
end

module type Lifted_constant = sig
  (** Description of a statically-allocated constant discovered during
      simplification. *)

  type downwards_env

  type t

  val print : Format.formatter -> t -> unit

  (** [create] takes the types of symbols to avoid re-inferring them. *)
  val create
     : downwards_env
    -> Flambda.Let_symbol_expr.Bound_symbols.t
    -> Flambda.Static_const.t
    -> types_of_symbols:Flambda_type.t Symbol.Map.t
    -> t

  val create_piece_of_code
     : downwards_env
    -> ?newer_version_of:Code_id.t
    -> Code_id.t
    -> Flambda.Function_params_and_body.t
    -> t

  val create_pieces_of_code
     : downwards_env
    -> ?newer_versions_of:Code_id.t Code_id.Map.t
    -> Flambda.Function_params_and_body.t Code_id.Map.t
    -> t

  val denv_at_definition : t -> downwards_env
  val bound_symbols : t -> Flambda.Let_symbol_expr.Bound_symbols.t
  val defining_expr : t -> Flambda.Static_const.t
  val types_of_symbols : t -> Flambda_type.t Symbol.Map.t
end
