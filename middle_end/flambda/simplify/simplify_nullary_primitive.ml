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

let simplify_nullary_primitive dacc (prim : P.nullary_primitive)
      dbg ~result_var =
  let result_var' = Var_in_binding_pos.var result_var in
  let original_prim : P.t = Nullary prim in
  let original_term = Named.create_prim original_prim dbg in
  match prim with
  | Probe_is_enabled _ ->
    let ty = T.any_tagged_bool () in
    let env_extension = TEE.one_equation (Name.var result_var') ty in
    Reachable.reachable original_term, env_extension, dacc
