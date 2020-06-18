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

(** Simplification functions on [Simple.t]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val simplify_simple
   : Downwards_acc.t
  -> Simple.t
  -> min_name_mode:Name_mode.t
  -> Simple.t Or_bottom.t * Flambda_type.t

type changed =
  | Unchanged
  | Changed

val simplify_simples
   : Downwards_acc.t
  -> Simple.t list
  -> min_name_mode:Name_mode.t
  -> changed * ((Simple.t * Flambda_type.t) list Or_bottom.t)

val simplify_simples'
   : Downwards_acc.t
  -> Simple.t list
  -> min_name_mode:Name_mode.t
  -> changed * (Simple.t list Or_bottom.t)
