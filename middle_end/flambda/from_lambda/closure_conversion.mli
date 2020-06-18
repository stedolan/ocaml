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

(** Introduce closures into Ilambda code, producing Flambda. *)

val ilambda_to_flambda
   : backend:(module Flambda_backend_intf.S)
  -> module_ident:Ident.t
  -> module_block_size_in_words:int
  -> filename:string  (* CR mshinwell: Filename of what? *)
  -> Ilambda.program
  -> Flambda_unit.t
