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

(** The alpha-equivalence class of the binding of a list of parameters around
    an expression, forming a continuation handler, together with auxiliary
    information about such handler. *)
type t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

include Contains_ids.S with type t := t

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

val print_using_where_with_cache
   : Recursive.t
  -> cache:Printing_cache.t
  -> Format.formatter
  -> Continuation.t
  -> t
  -> first:bool
  -> unit
