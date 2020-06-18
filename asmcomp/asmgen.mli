(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** From Lambda to assembly code *)

(** The type of converters from Lambda to Clambda. *)
type middle_end =
     backend:(module Backend_intf.S)
  -> filename:string
  -> prefixname:string
  -> ppf_dump:Format.formatter
  -> Lambda.program
  -> Clambda.with_constants

(** Compile an implementation from Lambda using the given middle end. *)
val compile_implementation
   : ?toplevel:(string -> bool)
  -> backend:(module Backend_intf.S)
  -> filename:string
  -> prefixname:string
  -> middle_end:middle_end
  -> ppf_dump:Format.formatter
  -> Lambda.program
  -> unit

(** The type of converters from Lambda to Flambda programs *)
type middle_end_flambda =
     ppf_dump:Format.formatter
  -> prefixname:string
  -> backend:(module Flambda_backend_intf.S)
  -> filename:string
  -> module_ident:Ident.t
  -> module_block_size_in_words:int
  -> module_initializer:Lambda.lambda
  -> Flambda_middle_end.middle_end_result

(** Compile an implementation from Lambda using Flambda. *)
val compile_implementation_flambda
   : ?toplevel:(string -> bool)
  -> backend:(module Flambda_backend_intf.S)
  -> filename:string
  -> prefixname:string
  -> size:int
  -> module_ident:Ident.t
  -> module_initializer:Lambda.lambda
  -> middle_end:middle_end_flambda
  -> ppf_dump:Format.formatter
  -> required_globals:Ident.Set.t
  -> unit
  -> unit

(** Specialised version of [compile_implementation_flambda] for ilambdac. *)
val compile_implementation_flambda_for_ilambdac
   : ?toplevel:(string -> bool)
  -> prefixname:string
  -> ppf_dump:Format.formatter
  -> required_globals:Ident.Set.t
  -> Flambda_middle_end.middle_end_result
  -> unit

(** Information that Flambda needs to know about the backend. *)
module Flambda_backend : Flambda_backend_intf.S

val compile_phrase :
    ppf_dump:Format.formatter -> Cmm.phrase -> unit

type error = Assembler_error of string
exception Error of error
val report_error: Format.formatter -> error -> unit


val compile_unit:
  string(*asm file*) -> bool(*keep asm*) ->
  string(*obj file*) -> (unit -> unit) -> unit
