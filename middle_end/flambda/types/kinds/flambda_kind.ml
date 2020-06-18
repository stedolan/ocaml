(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2019 OCamlPro SAS                                    *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type value = private Value
type empty_naked_immediate = private Naked_immediate
type empty_naked_float = private Naked_float
type empty_naked_int32 = private Naked_int32
type empty_naked_int64 = private Naked_int64
type empty_naked_nativeint = private Naked_nativeint
type fabricated = private Fabricated

type naked_immediate = empty_naked_immediate * Target_imm.Set.t
type naked_float = empty_naked_float * Numbers.Float_by_bit_pattern.Set.t
type naked_int32 = empty_naked_int32 * Numbers.Int32.Set.t
type naked_int64 = empty_naked_int64 * Numbers.Int64.Set.t
type naked_nativeint = empty_naked_nativeint * Targetint.Set.t

module Naked_number_kind = struct
  type t =
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let print ppf t =
    match t with
    | Naked_immediate -> Format.pp_print_string ppf "Naked_immediate"
    | Naked_float -> Format.pp_print_string ppf "Naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"

  let to_int t =
    match t with
    | Naked_immediate -> 0
    | Naked_float -> 1
    | Naked_int32 -> 2
    | Naked_int64 -> 3
    | Naked_nativeint -> 4

  let compare t1 t2 =
    Int.compare (to_int t1) (to_int t2)
end

type t =
  | Value
  | Naked_number of Naked_number_kind.t
  | Fabricated

type kind = t

let value = Value
let naked_immediate = Naked_number Naked_immediate
let naked_float = Naked_number Naked_float
let naked_int32 = Naked_number Naked_int32
let naked_int64 = Naked_number Naked_int64
let naked_nativeint = Naked_number Naked_nativeint
let fabricated = Fabricated

let unit = Value

let unicode = true  (* CR mshinwell: move elsewhere *)

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    if t1 == t2 then 0
    else
      match t1, t2 with
      | Value, Value -> 0
      | Naked_number n1, Naked_number n2 -> Naked_number_kind.compare n1 n2
      | Fabricated, Fabricated -> 0
      | Value, _ -> -1
      | _, Value -> 1
      | Naked_number _, _ -> -1
      | _, Naked_number _ -> 1

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash = Hashtbl.hash

  let print ppf t =
    let colour = Flambda_colours.kind () in
    match t with
    | Value ->
      if unicode then
        Format.fprintf ppf "@<0>%s@<1>\u{1d54d}@<0>%s" colour
          (Flambda_colours.normal ())
      else
        Format.fprintf ppf "Val"
    | Naked_number naked_number_kind ->
      if unicode then begin
        match naked_number_kind with
        | Naked_immediate ->
          Format.fprintf ppf "@<0>%s@<1>\u{2115}@<1>\u{1d55a}@<0>%s"
            colour (Flambda_colours.normal ())
        | Naked_float ->
          Format.fprintf ppf "@<0>%s@<1>\u{2115}@<1>\u{1d557}@<0>%s"
            colour (Flambda_colours.normal ())
        | Naked_int32 ->
          Format.fprintf ppf "@<0>%s@<1>\u{2115}@<1>\u{1d7db}@<1>\u{1d7da}@<0>%s"
            colour (Flambda_colours.normal ())
        | Naked_int64 ->
          Format.fprintf ppf "@<0>%s@<1>\u{2115}@<1>\u{1d7de}@<1>\u{1d7dc}@<0>%s"
            colour (Flambda_colours.normal ())
        | Naked_nativeint ->
          Format.fprintf ppf "@<0>%s@<1>\u{2115}@<1>\u{2115}@<0>%s"
            colour (Flambda_colours.normal ())
      end else begin
        Format.fprintf ppf "(Naked_number %a)"
          Naked_number_kind.print naked_number_kind
      end
    | Fabricated ->
      if unicode then
        Format.fprintf ppf "@<0>%s@<1>\u{1d53d}@<0>%s"
          colour (Flambda_colours.normal ())
      else
        Format.fprintf ppf "Fab"

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

let is_value t =
  match t with
  | Value -> true
  | Naked_number _
  | Fabricated -> false

let is_naked_float t =
  match t with
  | Naked_number Naked_float -> true
  | Value
  | Naked_number _
  | Fabricated -> false

module Standard_int = struct
  type t =
    | Tagged_immediate
    | Naked_immediate
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let to_int t =
    match t with
    | Tagged_immediate -> 0
    | Naked_immediate -> 1
    | Naked_int32 -> 2
    | Naked_int64 -> 3
    | Naked_nativeint -> 4

  let to_kind t : kind =
    match t with
    | Tagged_immediate -> Value
    | Naked_immediate -> Naked_number Naked_immediate
    | Naked_int32 -> Naked_number Naked_int32
    | Naked_int64 -> Naked_number Naked_int64
    | Naked_nativeint -> Naked_number Naked_nativeint

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Tagged_immediate -> Format.pp_print_string ppf "Tagged_immediate"
      | Naked_immediate -> Format.pp_print_string ppf "Naked_immediate"
      | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
      | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
      | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"

    let output chan t =
      print (Format.formatter_of_out_channel chan) t

    let compare t1 t2 = (to_int t1) - (to_int t2)

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash = Hashtbl.hash
  end)

  let print_lowercase ppf t =
    match t with
    | Tagged_immediate -> Format.pp_print_string ppf "tagged_immediate"
    | Naked_immediate -> Format.pp_print_string ppf "naked_immediate"
    | Naked_int32 -> Format.pp_print_string ppf "naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "naked_nativeint"
end

module Standard_int_or_float = struct
  type t =
    | Tagged_immediate
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  let to_int t =
    match t with
    | Tagged_immediate -> 0
    | Naked_immediate -> 1
    | Naked_float -> 2
    | Naked_int32 -> 3
    | Naked_int64 -> 4
    | Naked_nativeint -> 5

  let to_kind t : kind =
    match t with
    | Tagged_immediate -> Value
    | Naked_immediate -> Naked_number Naked_immediate
    | Naked_float -> Naked_number Naked_float
    | Naked_int32 -> Naked_number Naked_int32
    | Naked_int64 -> Naked_number Naked_int64
    | Naked_nativeint -> Naked_number Naked_nativeint

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Tagged_immediate -> Format.pp_print_string ppf "Tagged_immediate"
      | Naked_immediate -> Format.pp_print_string ppf "Naked_immediate"
      | Naked_float -> Format.pp_print_string ppf "Naked_float"
      | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
      | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
      | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"

    let output chan t =
      print (Format.formatter_of_out_channel chan) t

    let compare t1 t2 = (to_int t1) - (to_int t2)
    let equal t1 t2 = (compare t1 t2 = 0)
    let hash = Hashtbl.hash
  end)

  let print_lowercase ppf t =
    match t with
    | Tagged_immediate -> Format.pp_print_string ppf "tagged_immediate"
    | Naked_immediate -> Format.pp_print_string ppf "naked_immediate"
    | Naked_float -> Format.pp_print_string ppf "naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "naked_nativeint"
end

module Boxable_number = struct
  type t =
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint
    | Untagged_immediate

  let to_int t =
    match t with
    | Naked_float -> 0
    | Naked_int32 -> 1
    | Naked_int64 -> 2
    | Naked_nativeint -> 3
    | Untagged_immediate -> 4

  let to_kind t : kind =
    match t with
    | Naked_float -> Naked_number Naked_float
    | Naked_int32 -> Naked_number Naked_int32
    | Naked_int64 -> Naked_number Naked_int64
    | Naked_nativeint -> Naked_number Naked_nativeint
    | Untagged_immediate -> Naked_number Naked_immediate

  let of_naked_number_kind k : t =
    match (k : Naked_number_kind.t) with
    | Naked_immediate -> Untagged_immediate
    | Naked_float -> Naked_float
    | Naked_int32 -> Naked_int32
    | Naked_int64 -> Naked_int64
    | Naked_nativeint -> Naked_nativeint

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Naked_float -> Format.pp_print_string ppf "Naked_float"
      | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
      | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
      | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"
      | Untagged_immediate -> Format.pp_print_string ppf "Untagged_immediate"

    let output chan t =
      print (Format.formatter_of_out_channel chan) t

    let compare t1 t2 = (to_int t1) - (to_int t2)

    let equal t1 t2 = (t1 == t2)

    let hash = Hashtbl.hash
  end)

  let print_lowercase ppf t =
    match t with
    | Naked_float -> Format.pp_print_string ppf "naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "naked_nativeint"
    | Untagged_immediate -> Format.pp_print_string ppf "untagged_immediate"

  let print_lowercase_short ppf t =
    match t with
    | Naked_float -> Format.pp_print_string ppf "float"
    | Naked_int32 -> Format.pp_print_string ppf "int32"
    | Naked_int64 -> Format.pp_print_string ppf "int64"
    | Naked_nativeint -> Format.pp_print_string ppf "nativeint"
    | Untagged_immediate -> Format.pp_print_string ppf "untagged_imm"
end

module Naked_number = struct
  type 'k t =
    | Naked_immediate : naked_immediate t
    | Naked_float : naked_float t
    | Naked_int32 : naked_int32 t
    | Naked_int64 : naked_int64 t
    | Naked_nativeint : naked_nativeint t

  let print (type a) ppf (t : a t) =
    match t with
    | Naked_immediate -> Format.pp_print_string ppf "Naked_immediate"
    | Naked_float -> Format.pp_print_string ppf "Naked_float"
    | Naked_int32 -> Format.pp_print_string ppf "Naked_int32"
    | Naked_int64 -> Format.pp_print_string ppf "Naked_int64"
    | Naked_nativeint -> Format.pp_print_string ppf "Naked_nativeint"
end
