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

type t =
  | Reachable of Named.t
  | Invalid of Invalid_term_semantics.t

let reachable named = Reachable named

let invalid () =
  if !Clflags.treat_invalid_code_as_unreachable then
    Invalid Treat_as_unreachable
  else
    Invalid Halt_and_catch_fire

let print ppf t =
  match t with
  | Reachable named -> Named.print ppf named
  | Invalid sem -> Invalid_term_semantics.print ppf sem

let is_invalid t =
  match t with
  | Reachable _ -> false
  | Invalid _ -> true
