(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type 'a t =
  | Ok of 'a
  | Bottom
  | Absorbing

val print
   : (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a t
  -> unit

(* CR mshinwell: Misleading name, given the presence of [f]. *)
val of_or_bottom : 'a Or_bottom.t -> f:('a -> 'b) -> 'b t

val map : 'a t -> f:('a -> 'b) -> 'b t
