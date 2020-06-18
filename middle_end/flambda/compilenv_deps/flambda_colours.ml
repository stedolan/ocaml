(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module C = Misc.Color

let normal () = C.reset ()

let prim_constructive () = C.fg_256 163
let prim_destructive () = C.fg_256 62
let prim_neither () = C.fg_256 130

let naked_number () = C.fg_256 70
let tagged_immediate () = C.fg_256 70
let constructor () = C.fg_256 69

let kind () = C.fg_256 37

let top_or_bottom_type () = C.fg_256 37

let debuginfo () = C.fg_256 243

let discriminant () = C.fg_256 111
let name () = C.fg_256 111
let parameter () = C.fg_256 198
let symbol () = C.fg_256 98
let variable () = C.fg_256 111

let closure_element () = C.fg_256 31
let closure_var () = C.fg_256 43

let code_id () = C.fg_256 169

let expr_keyword () = C.fg_256 51
let static_keyword () = (C.fg_256 255) ^ (C.bg_256 240)

let static_part () = (C.fg_256 255) ^ (C.bg_256 237)

let continuation () = C.fg_256 35
let continuation_definition () = C.bg_256 237
let continuation_annotation () = (C.fg_256 202) ^ (C.bg_256 237)

let name_abstraction () = C.fg_256 172

let rec_info () = C.fg_256 243

let error () = C.fg_256 160

let elide () = C.fg_256 243

let each_file () = C.fg_256 51

let lambda () = expr_keyword ()
