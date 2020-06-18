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

(** Construct terms using only information from types. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val create_static_const : Flambda_type.to_lift -> Flambda.Static_const.t

val try_to_reify
   : Downwards_acc.t
  -> Reachable.t
  -> bound_to:Var_in_binding_pos.t
  -> Reachable.t * Downwards_acc.t * Flambda_type.t
