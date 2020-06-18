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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import

type failure =
  | Division_by_zero
  | Index_out_of_bounds

type expr_primitive =
  | Simple of Simple.t
  | Unary of Flambda_primitive.unary_primitive * simple_or_prim
  | Binary of Flambda_primitive.binary_primitive
      * simple_or_prim * simple_or_prim
  | Ternary of Flambda_primitive.ternary_primitive
      * simple_or_prim * simple_or_prim * simple_or_prim
  | Variadic of Flambda_primitive.variadic_primitive * (simple_or_prim list)
  | Checked of { validity_conditions : expr_primitive list;
                 (** The [validity_conditions] return untagged immediates
                     representing boolean values. *)
                 primitive : expr_primitive;
                 failure : failure; (* Predefined exception *)
                 dbg : Debuginfo.t }

and simple_or_prim =
  | Simple of Simple.t
  | Prim of expr_primitive

val print_expr_primitive : Format.formatter -> expr_primitive -> unit

val print_simple_or_prim
   : Format.formatter
  -> simple_or_prim
  -> unit

val print_list_of_simple_or_prim
   : Format.formatter
  -> simple_or_prim list
  -> unit

val expression_for_failure
   : backend:(module Flambda_backend_intf.S)
  -> Exn_continuation.t option
  -> register_const_string:(string -> Symbol.t)
  -> expr_primitive
  -> Debuginfo.t
  -> failure
  -> Expr.t

val bind_rec
   : backend:(module Flambda_backend_intf.S)
  -> Exn_continuation.t option
  -> register_const_string:(string -> Symbol.t)
  -> expr_primitive
  -> Debuginfo.t
  -> (Named.t -> Expr.t * Delayed_handlers.t)
  -> Expr.t * Delayed_handlers.t
