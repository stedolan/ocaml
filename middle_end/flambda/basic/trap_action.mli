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

(** Actions affecting exception traps on the stack.  These are always
    associated with an [Apply_cont] node; the trap action is executed before
    the application of the continuation.

    [Pop] may not appear to need the [exn_handler] value during Flambda
    passes---but in fact it does, since it compiles to a reference to such
    continuation, and must not be moved out of its scope.

    Beware: continuations cannot be used both as an exception handler and as
    a normal continuation (since continuations used as exception handlers
    use a calling convention that may differ from normal).
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type raise_kind =
  | Regular
  | Reraise
  | No_trace

type t =
  | Push of { exn_handler : Continuation.t; }
  | Pop of {
      exn_handler : Continuation.t;
      raise_kind : raise_kind option;
    }

include Expr_std.S with type t := t

module Option : sig
  type nonrec t = t option

  val print : Format.formatter -> t -> unit
end

val compare : t -> t -> int
