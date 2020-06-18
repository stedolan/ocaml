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

open! Simplify_import

let simplify_primitive dacc ~original_named (prim : P.t) dbg ~result_var =
(*Format.eprintf "Simplifying primitive:@ %a\n%!" P.print prim;*)
  match prim with
  | Unary (prim, arg) ->
    Simplify_unary_primitive.simplify_unary_primitive dacc
      prim arg dbg ~result_var
  | Binary (prim, arg1, arg2) ->
    Simplify_binary_primitive.simplify_binary_primitive dacc
      prim arg1 arg2 dbg ~result_var
  | Ternary (prim, arg1, arg2, arg3) ->
    Simplify_ternary_primitive.simplify_ternary_primitive dacc
      prim arg1 arg2 arg3 dbg ~result_var
  | Variadic (variadic_prim, args) ->
    Simplify_variadic_primitive.simplify_variadic_primitive dacc
      ~original_named ~original_prim:prim variadic_prim
      args dbg ~result_var
