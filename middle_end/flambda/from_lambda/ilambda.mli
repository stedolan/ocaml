(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2019 OCamlPro SAS                                    *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Ilambda: halfway between Lambda and Flambda. In CPS but without closures and
    still permitting mutable variables.

    This language is used only as an internal language for communication between
    the CPS and closure conversion passes. We don't do any transformations on
    the language save for mutable variable elimination, so there is little
    abstraction, and no features such as advanced treatment of name binding.

    Flambda expressions augment Ilambda expressions by adding constructs for:
    - the construction and manipulation of closures; and
    - accessing constants that have been lifted to static data.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: We need to stop primitives having control flow effects, e.g.
   asynchronous exceptions from finalisers, really.
   Although there's still the problem with signal handlers and Ctrl+C.  Maybe
   we should have allocation as a distinguished "named". *)

type simple =
  | Var of Ident.t
  | Const of Lambda.structured_constant

type exn_continuation =
  { exn_handler : Continuation.t;
    extra_args : (simple * Lambda.value_kind) list;
  }

type trap_action =
  | Push of { exn_handler : Continuation.t; }
  | Pop of { exn_handler : Continuation.t; }

type user_visible =
  | User_visible
  | Not_user_visible

type t =
  | Let of Ident.t * user_visible * Lambda.value_kind * named * t
  | Let_mutable of let_mutable
  | Let_rec of function_declarations * t
    (** Value "let rec" has already been expanded by [Prepare_lambda]. *)
  | Let_cont of let_cont
  | Apply of apply
  | Apply_cont of Continuation.t * trap_action option * simple list
    (** Unlike in Flambda, [Apply_cont] is not used for the raising of
        exceptions; use [Prim Praise] instead. *)
  | Switch of Ident.t * switch

and named =
  | Simple of simple
  | Prim of {
      prim : Lambda.primitive;
      args : simple list;
      loc : Lambda.scoped_location;
      exn_continuation : exn_continuation option;
    }
    (** Set [exn_continuation] to [None] iff the given primitive can never
        raise. *)
  | Assign of { being_assigned : Ident.t; new_value : simple; }

and let_mutable = {
  id : Ident.t;
  initial_value : simple;
  contents_kind : Lambda.value_kind;
  body : t;
}

and function_declaration = {
  kind : Lambda.function_kind;
  return_continuation : Continuation.t;
  exn_continuation : exn_continuation;
  params : (Ident.t * Lambda.value_kind) list;
  return : Lambda.value_kind;
  body : t;
  free_idents_of_body : Ident.Set.t;
  attr : Lambda.function_attribute;
  loc : Lambda.scoped_location;
  stub : bool;
}

and function_declarations = (Ident.t * function_declaration) list

and let_cont = {
  name : Continuation.t;
  is_exn_handler : bool;
  (* CR mshinwell: update comment *)
  (** Continuations that are exception handlers must be [Non_recursive] and
      have exactly one parameter. *)
  params : (Ident.t * user_visible * Lambda.value_kind) list;
  recursive : Asttypes.rec_flag; (* CR mshinwell: Recursive.t *)
  body : t;
  handler : t;
}

and apply = {
  kind : apply_kind;
  func : Ident.t;
  args : simple list;
  continuation : Continuation.t;
  exn_continuation : exn_continuation;
  loc : Lambda.scoped_location;
  should_be_tailcall : bool;
  inlined : Lambda.inline_attribute;
  specialised : Lambda.specialise_attribute;
}

and apply_kind =
  | Function
  | Method of { kind : Lambda.meth_kind; obj : simple; }

and switch = {
  numconsts : int;
  consts : (int * Continuation.t * trap_action option * (simple list)) list;
  failaction : (Continuation.t * trap_action option * (simple list)) option;
}

type program = {
  expr : t;
  return_continuation : Continuation.t;
  exn_continuation : exn_continuation;
  uses_mutable_variables : bool;
}

val print : Format.formatter -> t -> unit
val print_named : Format.formatter -> named -> unit
val print_program : Format.formatter -> program -> unit

val recursive_functions : function_declarations -> Ident.Set.t

val contains_closures : t -> bool
