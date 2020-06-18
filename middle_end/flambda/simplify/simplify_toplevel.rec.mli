(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val simplify_toplevel
   : Downwards_acc.t
  -> Flambda.Expr.t
  -> return_continuation:Continuation.t
  -> return_arity:Flambda_arity.t
  -> Exn_continuation.t
  -> return_cont_scope:Scope.t
  -> exn_cont_scope:Scope.t
  -> Flambda.Expr.t * Downwards_acc.t * Simplify_env_and_result.Result.t
