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

type t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

include Contains_ids.S with type t := t

(** Create a function declaration. *)
val create
   : code_id:Code_id.t
  -> params_arity:Flambda_arity.t
  -> result_arity:Flambda_arity.t
  -> stub:bool
  -> dbg:Debuginfo.t
  -> inline:Inline_attribute.t
  -> is_a_functor:bool
  -> recursive:Recursive.t
  -> t

(** The identifier of the code of the function (which must be bound using
    [Define_symbol]). *)
val code_id : t -> Code_id.t

(* CR mshinwell: Be consistent: "param_arity" or "params_arity" throughout. *)
val params_arity : t -> Flambda_arity.t

(** The arity of the return continuation of the function.  This provides the
    number of results that the function produces and their kinds. *)
(* CR mshinwell: Be consistent everywhere as regards "result" vs "return"
   arity. *)
val result_arity : t -> Flambda_arity.t

(** A stub function is a generated function used to prepare arguments or
    return values to allow indirect calls to functions with a special
    calling convention.  For instance indirect calls to tuplified functions
    must go through a stub.  Stubs will be unconditionally inlined. *)
val stub : t -> bool

(** Debug info for the function declaration. *)
val dbg : t -> Debuginfo.t

(** Inlining requirements from the source code. *)
val inline : t -> Inline_attribute.t

(** Whether the function is known definitively to be a functor. *)
val is_a_functor : t -> bool

(** Return a function declaration that is like the supplied one except
    that it has a new code ID. *)
val update_code_id : t -> Code_id.t -> t

(** Whether the function is recursive, in the sense of the syntactic analysis
    conducted during closure conversion. *)
val recursive : t -> Recursive.t

val compare : t -> t -> int

val equal : t -> t -> bool
