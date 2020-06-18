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

include Numbers.Int
type binding_time = t

let strictly_earlier (t : t) ~than =
  t < than

let consts_and_discriminants = 0
let symbols = 1
let imported_variables = 2
let earliest_var = 3

let succ (t : t) =
  if t < earliest_var then
    Misc.fatal_error "Cannot increment binding time for symbols"
  else
    t + 1

(* CR mshinwell: enforce an upper limit on values of type [t] *)

module With_name_mode = struct
  type t = int

  let [@inline always] create binding_time name_mode =
    let name_mode =
      match Name_mode.descr name_mode with
      | Normal -> 0
      | In_types -> 1
      | Phantom -> 2
    in
    (binding_time lsl 2) lor name_mode

  let binding_time t =
    t lsr 2

  let [@inline always] name_mode t =
    match t land 3 with
    | 0 -> Name_mode.normal
    | 1 -> Name_mode.in_types
    | 2 -> Name_mode.phantom
    | _ -> assert false

  let print ppf t =
    Format.fprintf ppf "(bound at time %d %a)" (binding_time t)
      Name_mode.print (name_mode t)

  let equal t1 t2 =
    t1 = t2
end
