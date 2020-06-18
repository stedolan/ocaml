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

(** Environments and result structures used during simplification. *)

(* CR mshinwell: This module is a nuisance -- we should split it across
   files. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module rec Downwards_env : (Simplify_env_and_result_intf.Downwards_env
  with type result := Result.t
  with type lifted_constant := Lifted_constant.t)
and Upwards_env : (Simplify_env_and_result_intf.Upwards_env
  with type downwards_env := Downwards_env.t)
and Result : (Simplify_env_and_result_intf.Result
  with type lifted_constant := Lifted_constant.t)
and Lifted_constant : (Simplify_env_and_result_intf.Lifted_constant
  with type downwards_env := Downwards_env.t)
