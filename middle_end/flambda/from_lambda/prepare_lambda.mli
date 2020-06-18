(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2019 OCamlPro SAS                                    *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Preparation of [Lambda] code before CPS and closure conversion. *)

(** The set of integers returned by [run] identifies all those [Lstaticcatch]
    handlers which are to be treated as recursive.  (This is rather more
    straightforward than changing the type in [Lambda] to accommodate
    this). *)
val run
   : Lambda.lambda
  -> Lambda.lambda * Numbers.Int.Set.t

val stub_hack_prim_name : string
