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

val inline
   : Downwards_acc.t
  -> callee:Simple.t
  -> args:Simple.t list
  -> Flambda_type.Function_declaration_type.Inlinable.t
  -> apply_return_continuation:Apply.Result_continuation.t
  -> apply_exn_continuation:Exn_continuation.t
  -> apply_inlining_depth:int
  -> unroll_to:int option
  -> Debuginfo.t
  -> Downwards_acc.t * Expr.t
