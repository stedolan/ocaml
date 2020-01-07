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

(* Environment for flambda2 to cmm translation *)

(** {1 Translation environment} *)

type t
(** Environment for flambda2 to cmm translation *)

val mk :
  Exported_offsets.t ->
  Continuation.t -> Continuation.t ->
  Var_within_closure.Set.t -> t
(** [mk offsets k k_exn used_closure_vars] creates a local environment for
    translating a flambda2 expression, with return continuation [k], exception
    continuation [k_exn], and which uses the given closures variables. *)

val enter_function_def : t -> Continuation.t -> Continuation.t -> t
(** [enter_function_def env k k_exn] creates a local environment for
    translating a flambda2 expression, with return continuation [k], exception
    continuation [k_exn], preserving the global info from [env]. *)

val dummy : Exported_offsets.t -> Var_within_closure.Set.t -> t
(** Create an environment with dummy return adn exception continuations. *)


(** {2 Continuations} *)

val return_cont : t -> Continuation.t
(** Returns the return continuation of the environment. *)

val exn_cont : t -> Continuation.t
(** Returns the exception continuation of the environment. *)


(** {2 Function info *)

type function_info = {
  needs_closure_arg : bool;
  (* Whether direct calls need to provide a closure or can skip it *)
}

val add_function_info : t -> Code_id.t -> function_info -> t
(** Add information on the given function *)

val get_function_info : t -> Code_id.t -> function_info option
(** Retrieve known information on the given function *)


(** {2 Variable bindings} *)

type extra_info =
  | Untag of Cmm.expression
  (** The variable is bound to the result of untagging the cmm expression.
      This allows to have access to the cmm expression before untagging. *)
(** Extra information about bound variables. These are not necessary
    for the translation, but useful to enable certain optimization. *)

val create_variable : t -> Variable.t -> t * Backend_var.With_provenance.t
(** Create (and bind) a cmm variable for the given flambda2 variable, and return
    the new environment, and the created variable. Will fail (i.e. assertion
    failure) if the given variable is already bound. *)

val create_variables : t -> Variable.t list -> t * Backend_var.With_provenance.t list
(** Same as {!create_variable} but for a list of variables. *)

val bind_variable :
  t -> Variable.t ->
  ?extra:extra_info ->
  Effects_and_coeffects.t ->
  bool -> Cmm.expression -> t
(** Bind a variable to the given cmm expression, to allow for delaying the let-binding. *)

val get_variable : t -> Variable.t -> Cmm.expression
(** Get the cmm variable bound to a flambda2 variable.
    Will fail (i.e. assertion failure) if the variable is not bound.
    Be careful: in general you do *NOT* want to use this function but
    instead the {inline_variable} function, as it will correctly
    perform the inlining of used exactly once variables. *)

val inline_variable : t -> Variable.t -> Cmm.expression * t * Effects_and_coeffects.t
(** Try and inline an flambda2 variable using the delayed let-bindings. *)

val flush_delayed_lets : t -> (Cmm.expression -> Cmm.expression) * t
(** Wrap the given cmm expression with all the delayed let bindings accumulated
    in the environment. *)

val extra_info : t -> Variable.t -> extra_info option
(** Fetch the extra info for a flambda variable (if any). *)


(** {2 Continuation bindings} *)

type cont =
  | Jump of { types: Cmm.machtype list; cont: int; }
  (** Static jump, with the given cmm continuation.
      The list of machtypes represent the types of arguments expected by the
      catch handler. *)
  | Inline of { handler_params: Kinded_parameter.t list;
                handler_body: Flambda.Expr.t; }
  (** Inline the continuation.
      When inlining is not possible, generate a jump *)
(** Translation information for continuations. A continuation may either
    be translated as a static jump, or inlined at its call site. *)

val add_jump_cont : t -> Cmm.machtype list -> Continuation.t -> int * t
(** Bind the given continuation to a jump, creating a fresh jump id for it. *)

val add_inline_cont :
  t -> Continuation.t -> Kinded_parameter.t list -> Flambda.Expr.t -> t
(** Bind the given continuation as an inline continuation, bound over
    the given variables.
    Returns the Cmm continuation id, a reference that will be set to true if
    a catch handler is needed, and the environment. *)

val add_exn_handler :
  t -> Continuation.t -> Flambda.Continuation_handler.t
  -> t * (Backend_var.t * Flambda_kind.t) list
(** Setup the extra mutable variables needed if the handler has extra arguments *)

val get_exn_extra_args :
  t -> Continuation.t -> Backend_var.t list
(** Recover the mutable variables associated with the given continuation *)

val get_k : t -> Continuation.t -> cont
(** Return the binding for a given continuation. Will fail
    (i.e. assertion failure) if given an unbound continuation. *)

val get_jump_id : t -> Continuation.t -> int
(** Returns the jump id bound to a continuation. Will fail (assertion failure),
    if the continuation is not bound. *)


(** {2 Sets of closures and offsets} *)

val closure_offset : t -> Closure_id.t -> Exported_offsets.closure_info option
(** Wrapper around {!Un_cps_closure.closure_offset}. *)

val env_var_offset : t -> Var_within_closure.t -> Exported_offsets.env_var_info option
(** Wrapper around {!Un_cps_closure.env_var_offset}. *)

val layout :
  t -> Closure_id.t list -> Var_within_closure.t list -> Un_cps_closure.layout
(** Wrapper around {!Un_cps_closure.layout}. *)

val used_closure_vars : t -> Var_within_closure.Set.t
(** All closure variables used in the whole program. *)

val add_to_scope : t -> Code_id_or_symbol.Set.t -> t
(** Add the given names to the current scope *)

val mark_code_id_as_deleted : t -> Code_id.t -> t
(** Mark the given code id as deleted, so that [check_scope] will report
    an error if it is used. *)

val check_scope : allow_deleted:bool -> t -> Code_id_or_symbol.t -> t
(** Check that the given name is in scope. If [allow_deleted] is [false],
    check that it is not declared as deleted. *)

