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

(* CR mshinwell: Work out how to auto-generate this file. *)

(** The terms of the intermediate language used for tree-based analysis and
    optimisation.
*)

module Apply = Apply_expr
module Apply_cont = Apply_cont_expr
module Switch = Switch_expr

(** The basic structure of the language ensures that:
    - every intermediate value (and in particular every potential constant
      that we may want to lift) has a name;
    - every point to which we might wish to jump has a name;
    - there are no nested "let"s or subexpressions;
    - no re-normalisation of terms is required when substituting an
      application for an inlined body (unlike in ANF form).
*)
module rec Expr : sig
  (** The type of equivalence classes of expressions up to alpha-renaming of
      bound [Variable]s and [Continuation]s. *)
  type t

  (** Printing, invariant checks, name manipulation, etc. *)
  include Expr_std.S with type t := t

  type descr = private
    | Let of Let_expr.t
    (** Bind a variable.  There can be no effect on control flow (save for
        asynchronous operations such as the invocation of finalisers or
        signal handlers as a result of reaching a safe point). *)
    | Let_symbol of Let_symbol_expr.t
    (** Bind code and/or data symbol(s).  This form of expression is only
        allowed in certain "toplevel" contexts.  The bound symbols are not
        treated up to alpha conversion; each such bound symbol must be
        unique across the whole program being compiled. *)
    | Let_cont of Let_cont_expr.t
    (** Define one or more continuations. *)
    | Apply of Apply.t
    (** Call an OCaml function, external function or method. *)
    | Apply_cont of Apply_cont.t
    (** Call a continuation, optionally adding or removing exception trap
        frames from the stack, which thus allows for the raising of
        exceptions. *)
    | Switch of Switch.t
    (** Conditional control flow. *)
    | Invalid of Invalid_term_semantics.t
    (** Code proved type-incorrect and therefore unreachable. *)

  (** Extract the description of an expression. *)
  val descr : t -> descr

  (** What happened when a [Let]-expression was created. *)
  type let_creation_result = private
    | Have_deleted of Named.t
    | Nothing_deleted

  (** Create a [Let]-expression.  Unnecessary variable bindings will not be
      created and their associated defining expressions will be reported as
      [Have_deleted]. *)
  val create_pattern_let0
     : Bindable_let_bound.t
    -> Named.t
    -> t
    -> t * let_creation_result

  (** Like [create_let0], but for use when the caller isn't interested in
      whether something got deleted. *)
  val create_let : Var_in_binding_pos.t -> Named.t -> t -> t

  (** Create a [Let]-expression that may bind more than a single [Variable]
      (such as is required to bind a [Set_of_closures]). *)
  val create_pattern_let : Bindable_let_bound.t -> Named.t -> t -> t

  (** Create a [Let_symbol] expression that binds a statically-allocated
      value to a symbol. *)
  val create_let_symbol : Let_symbol_expr.t -> t

  (** Create an application expression. *)
  val create_apply : Apply.t -> t

  (** Create a continuation application (in the zero-arity case, "goto"). *)
  val create_apply_cont : Apply_cont.t -> t

  (** What happened when a [Switch]-expression was created. *)
  type switch_creation_result = private
    | Have_deleted_comparison_but_not_branch
    | Have_deleted_comparison_and_branch
    | Nothing_deleted

  (** Create a [Switch] expression, save that zero-arm switches are converted
      to [Invalid], and one-arm switches to [Apply_cont]. *)
  val create_switch0
     : scrutinee:Simple.t
    -> arms:Apply_cont_expr.t Target_imm.Map.t
    -> Expr.t * switch_creation_result

  (** Like [create_switch0], but for use when the caller isn't interested in
      whether something got deleted. *)
  val create_switch
     : scrutinee:Simple.t
    -> arms:Apply_cont_expr.t Target_imm.Map.t
    -> Expr.t

  (** Build a [Switch] corresponding to a traditional if-then-else. *)
  val create_if_then_else
     : scrutinee:Simple.t
    -> if_true:Apply_cont_expr.t
    -> if_false:Apply_cont_expr.t
    -> t

  (** Create an expression indicating type-incorrect or unreachable code. *)
  val create_invalid : unit -> t

  (** [bind [var1, expr1; ...; varN, exprN] body] binds using
      [Immutable] [Let] expressions the given [(var, expr)] pairs around the
      body. *)
  val bind
     : bindings:(Var_in_binding_pos.t * Named.t) list
    -> body:t
    -> t

  val bind_parameters
     : bindings:(Kinded_parameter.t * Named.t) list
    -> body:t
    -> t

  (** Given lists of kinded parameters [p_1; ...; p_n] and simples
      [s_1; ...; s_n], create an expression that surrounds the given
      expression with bindings of each [p_i] to the corresponding [s_i],
      such as is typically used when performing an inlining transformation. *)
  val bind_parameters_to_simples
     : bind:Kinded_parameter.t list
    -> target:Simple.t list
    -> t
    -> t
end and Named : sig
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

  (** Returns [true] iff the given expression is a set of closures. *)
  val is_set_of_closures : t -> bool
end and Let_expr : sig
  (** The alpha-equivalence classes of expressions that bind variables. *)
  type t

  (** Printing, invariant checks, name manipulation, etc. *)
  include Expr_std.S with type t := t

  (** The defining expression of the [Let]. *)
  val defining_expr : t -> Named.t

  (** Look inside the [Let] by choosing a member of the alpha-equivalence
      class. *)
  val pattern_match
     : t
    -> f:(bound_vars:Bindable_let_bound.t -> body:Expr.t -> 'a)
    -> 'a

  (** Look inside two [Let]s by choosing members of their alpha-equivalence
      classes, using the same bound variables for both. *)
  val pattern_match_pair
     : t
    -> t
    -> f:(bound_vars:Bindable_let_bound.t -> body1:Expr.t -> body2:Expr.t -> 'a)
    -> 'a
end and Let_symbol_expr : sig
  module Bound_symbols : sig
    module Code_and_set_of_closures : sig
      type t = {
        code_ids : Code_id.Set.t;
        closure_symbols : Symbol.t Closure_id.Map.t;
      }

      val print : Format.formatter -> t -> unit
    end

    type t =
      | Singleton of Symbol.t
        (** A binding of a single symbol of kind [Value]. *)
      | Sets_of_closures of Code_and_set_of_closures.t list
        (** A recursive binding of possibly multiple sets of closures with
            associated code. All code IDs and symbols named in the
            [Code_and_set_of_closures.t list] are in scope for _all_ associated
            [Static_const.code_and_set_of_closures list] values on the
            right-hand side of the corresponding [Let_symbol] expression.
            Despite the recursive nature of the binding, the elements in the
            [Code_and_set_of_closures.t list] must correspond elementwise to the
            elements in the corresponding [Static_const.code_and_set_of_closures
            list]. *)

    val being_defined : t -> Symbol.Set.t

    val code_being_defined : t -> Code_id.Set.t

    val closure_symbols_being_defined : t -> Symbol.Set.t

    val everything_being_defined : t -> Code_id_or_symbol.Set.t

    include Expr_std.S with type t := t
  end

  module Scoping_rule : sig
    type t =
      | Syntactic
      | Dominator
  end

  type t

  val create
     : Scoping_rule.t
    -> Bound_symbols.t
    -> Static_const.t
    -> Expr.t
    -> t

  val scoping_rule : t -> Scoping_rule.t

  val bound_symbols : t -> Bound_symbols.t

  val defining_expr : t -> Static_const.t

  val body : t -> Expr.t

  include Expr_std.S with type t := t

  (** If [newer_versions_of] maps [id1] to [id2] then [id1] is a newer
      version of [id2]. *)
  val pieces_of_code
     : ?newer_versions_of:Code_id.t Code_id.Map.t
    -> ?set_of_closures:(Symbol.t Closure_id.Map.t * Set_of_closures.t)
    -> Function_params_and_body.t Code_id.Map.t
    -> Bound_symbols.t * Static_const.t
end and Let_cont_expr : sig
  (** Values of type [t] represent alpha-equivalence classes of the definitions
      of continuations:
        let_cont [name] [args] = [handler] in [body]
      or using an alternative notation:
        [body]
        where [name] [args] = [handler]

      - Continuations are second-class.
      - Continuations do not capture variables.
      - Continuations may be (mutually-)recursive.

      It is an error to mark a continuation that might be recursive as
      non-recursive. The converse is safe.

      Note: any continuation used as an exception handler must be non-recursive
      by the point it reaches [Flambda_to_cmm]. (This means that it is
      permissible to introduce mutual recursion through stubs associated with
      such continuations, so long as [Simplify] is run afterwards to inline them
      out and turn the resulting single [Recursive] handler into a
      [Non_recursive] one. *)
  (* CR mshinwell: ensure the statement about [Flambda_to_cmm] still holds. *)
  type t = private
    | Non_recursive of {
        handler : Non_recursive_let_cont_handler.t;
        num_free_occurrences : Name_occurrences.Num_occurrences.t;
        (** [num_free_occurrences] can be used, for example, to decide whether
            to inline out a linearly-used continuation.  It will always be
            strictly greater than zero. *)
      }
    | Recursive of Recursive_let_cont_handlers.t

  (** Printing, invariant checks, name manipulation, etc. *)
  include Expr_std.S with type t := t

  (** Create a definition of a non-recursive continuation.  If the continuation
      does not occur free in the [body], then just the [body] is returned,
      without any enclosing [Let_cont]. *)
  val create_non_recursive
     : Continuation.t
    -> Continuation_handler.t
    -> body:Expr.t
    -> Expr.t

  (** Create a definition of a set of possibly-recursive continuations. *)
  val create_recursive
     : Continuation_handler.t Continuation.Map.t
    -> body:Expr.t
    -> Expr.t
end and Non_recursive_let_cont_handler : sig
  (** The representation of the alpha-equivalence class of the binding of a
      single non-recursive continuation handler over a body. *)
  type t

  (** Printing, invariant checks, name manipulation, etc. *)
  include Expr_std.S with type t := t

  (** Deconstruct a continuation binding to get the name of the bound
      continuation and the expression over which it is scoped. *)
  val pattern_match
     : t
    -> f:(Continuation.t -> body:Expr.t -> 'a)
    -> 'a

  (** Deconstruct two continuation bindings using the same name. *)
  val pattern_match_pair
     : t
    -> t
    -> f:(Continuation.t -> body1:Expr.t -> body2:Expr.t -> 'a)
    -> 'a

  (** Obtain the continuation itself (rather than the body over which it
      is scoped). *)
  val handler : t -> Continuation_handler.t
end and Continuation_handler : sig
  (** The alpha-equivalence class of the binding of a list of parameters around
      an expression, forming a continuation handler, together with auxiliary
      information about such handler. *)
  type t

  (** Printing, invariant checks, name manipulation, etc. *)
  include Expr_std.S with type t := t

  (** Create the representation of a single continuation handler. *)
  val create
     : params_and_handler:Continuation_params_and_handler.t
    -> stub:bool
    -> is_exn_handler:bool
    -> t

  (** The alpha-equivalence class of the continuation's parameters bound over
      its code. *)
  val params_and_handler : t -> Continuation_params_and_handler.t

  (** Whether the continuation is an exception handler.

      Continuations used as exception handlers are always [Non_recursive]. To
      enable identification of them in passes not invoked from [Simplify] (where
      they could be identified by looking at the [Apply_cont]s that reference
      them) they are marked explicitly.

      Continuations used as exception handlers may have more than one
      parameter (see [Exn_continuation]).

      (Relevant piece of background info: the backend cannot compile
      simultaneously-defined continuations when one or more of them is an
      exception handler.) *)
  val is_exn_handler : t -> bool

  (** Whether the continuation is a compiler-generated wrapper that should
      always be inlined. *)
  (* CR mshinwell: Remove the notion of "stub" and enhance continuation and
     function declarations to hold one or more wrappers themselves. *)
  val stub : t -> bool

  val arity : t -> Flambda_arity.t

  val with_params_and_handler : t -> Continuation_params_and_handler.t -> t

  type behaviour = private
    | Unreachable of { arity : Flambda_arity.t; }
    | Alias_for of { arity : Flambda_arity.t; alias_for : Continuation.t; }
    | Unknown of { arity : Flambda_arity.t; }

  val behaviour : t -> behaviour
end and Continuation_params_and_handler : sig
  (** The representation of the alpha-equivalence class of bindings of a list
      of parameters, with associated relations thereon, over the code of a
      continuation handler. *)
  type t

  (** Printing, invariant checks, name manipulation, etc. *)
  include Expr_std.S with type t := t

  (** Create a value of type [t] given information about a continuation
      handler. *)
  val create
     : Kinded_parameter.t list
    -> handler:Expr.t
    -> t

  (** Choose a member of the alpha-equivalence class to enable examination
      of the parameters, relations thereon and the code over which they
      are scoped. *)
  val pattern_match
     : t
    -> f:(Kinded_parameter.t list
      -> handler:Expr.t
      -> 'a)
    -> 'a

  (** Choose members of two bindings' alpha-equivalence classes using the same
      parameters. *)
  val pattern_match_pair
     : t
    -> t
    -> f:(Kinded_parameter.t list
      -> handler1:Expr.t
      -> handler2:Expr.t
      -> 'a)
    -> 'a

end and Recursive_let_cont_handlers : sig
  (** The representation of the alpha-equivalence class of a group of possibly
      (mutually-) recursive continuation handlers that are bound both over a
      body and their own handler code. *)
  type t

  (** Printing, invariant checks, name manipulation, etc. *)
  include Expr_std.S with type t := t

  (** Deconstruct a continuation binding to get the bound continuations,
      together with the expressions and handlers over which they are scoped. *)
  val pattern_match
     : t
    -> f:(body:Expr.t -> Continuation_handlers.t -> 'a)
    -> 'a

(** Deconstruct two continuation bindings using the same bound continuations. *)
  val pattern_match_pair
   : t
  -> t
  -> f:(body1:Expr.t
    -> body2:Expr.t
    -> Continuation_handlers.t
    -> Continuation_handlers.t
    -> 'a)
  -> 'a

end and Continuation_handlers : sig
  (** The result of pattern matching on [Recursive_let_cont_handlers]
      (see above). *)
  type t

  (** Obtain the mapping from continuation to handler. *)
  val to_map : t -> Continuation_handler.t Continuation.Map.t

  (** The domain of [to_map t]. *)
  val domain : t -> Continuation.Set.t

  (** Whether any of the continuations are exception handlers. *)
  val contains_exn_handler : t -> bool
end and Function_params_and_body : sig
  (** A name abstraction that comprises a function's parameters (together with
      any relations between them), the code of the function, and the
      [my_closure] variable.  It also includes the return and exception
      continuations.

      These values are bound using [Define_symbol] constructs
      (see [Flambda_static]).

      From the body of the function, accesses to variables within the closure
      need to go via a [Project_var] (from [my_closure]); accesses to any other
      simultaneously-defined functions need to go likewise via a
      [Select_closure]. *)
  type t

  (** Printing, invariant checks, name manipulation, etc. *)
  include Expr_std.S with type t := t

  include Contains_ids.S with type t := t

  (** Create an abstraction that binds the given parameters, with associated
      relations thereon, over the given body. *)
  val create
     : return_continuation:Continuation.t
    -> Exn_continuation.t
    -> Kinded_parameter.t list
    -> dbg:Debuginfo.t
    -> body:Expr.t
    -> my_closure:Variable.t
    -> t

  (** Choose a member of the alpha-equivalence class to enable examination
      of the parameters, relations thereon and the body over which they are
      scoped. *)
  val pattern_match
     : t
    -> f:(return_continuation:Continuation.t
        (** The continuation parameter of the function, i.e. to where we must
            jump once the result of the function has been computed. If the
            continuation takes more than one argument then the backend will
            compile the function so that it returns multiple values. *)
      -> Exn_continuation.t
        (** To where we must jump if application of the function raises an
            exception. *)
      -> Kinded_parameter.t list
      -> body:Expr.t
      -> my_closure:Variable.t
      -> 'a)
    -> 'a

  (** Choose members of the alpha-equivalence classes of two definitions using
      the same names for the return continuation, the exception continuation,
      the closure, and all parameters. *)
  val pattern_match_pair
     : t
    -> t
    -> f:(return_continuation:Continuation.t
        (** The continuation parameter of the function, i.e. to where we must
            jump once the result of the function has been computed. If the
            continuation takes more than one argument then the backend will
            compile the function so that it returns multiple values. *)
      -> Exn_continuation.t
        (** To where we must jump if application of the function raises an
            exception. *)
      -> Kinded_parameter.t list
      -> body1:Expr.t
      -> body2:Expr.t
      -> my_closure:Variable.t
      -> 'a)
    -> 'a

   (** Return the debuginfo associated *)
   val debuginfo : t -> Debuginfo.t

end and Static_const : sig
  (** Language terms that represent statically-allocated values, bound to
      symbols. *)

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

  (** The possibly-recursive declaration of pieces of code and any associated
      set of closures. *)
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
    | Block of Tag.Scannable.t * Mutability.t
        * (Field_of_block.t list)
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

  val get_pieces_of_code : t -> Static_const.Code.t Code_id.Map.t

  val get_pieces_of_code' : t -> Function_params_and_body.t Code_id.Map.t

  (** Returns [true] iff the given term does not contain any variables,
      which means that the corresponding value can be statically allocated,
      without any need to patch it afterwards. *)
  val is_fully_static : t -> bool

  val can_share : t -> bool

  val must_be_sets_of_closures : t -> Code_and_set_of_closures.t list
end

module Function_declaration = Function_declaration
module Function_declarations = Function_declarations
module Let = Let_expr
module Let_cont = Let_cont_expr
module Set_of_closures = Set_of_closures

(** The idea is that you should typically do "open! Flambda" at the top of
    files, thus bringing in the following standard set of module aliases. *)
module Import : sig
  module Apply = Apply
  module Apply_cont = Apply_cont
  module Continuation_handler = Continuation_handler
  module Continuation_handlers = Continuation_handlers
  module Continuation_params_and_handler = Continuation_params_and_handler
  module Expr = Expr
  module Function_declaration = Function_declaration
  module Function_declarations = Function_declarations
  module Function_params_and_body = Function_params_and_body
  module Let = Let
  module Let_cont = Let_cont
  module Let_symbol = Let_symbol_expr
  module Named = Named
  module Non_recursive_let_cont_handler = Non_recursive_let_cont_handler
  module Recursive_let_cont_handlers = Recursive_let_cont_handlers
  module Set_of_closures = Set_of_closures
  module Static_const = Static_const
  module Switch = Switch
end
