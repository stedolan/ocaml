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

let print f ppf t =
  match t with
  | Ok contents -> Format.fprintf ppf "@[(Ok %a)@]" f contents
  | Bottom -> Format.pp_print_string ppf "Bottom"
  | Absorbing -> Format.pp_print_string ppf "Absorbing"

let of_or_bottom (or_bottom : _ Or_bottom.t) ~f : _ t =
  match or_bottom with
  | Ok contents -> Ok (f contents)
  | Bottom -> Bottom

let map t ~f =
  match t with
  | Ok contents -> Ok (f contents)
  | Bottom -> Bottom
  | Absorbing -> Absorbing
