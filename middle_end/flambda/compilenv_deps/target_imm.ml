(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module TO = Targetint.OCaml

type 'a or_wrong =
  | Ok of 'a
  | Wrong

type t = {
  value : TO.t;
  print_as_char : bool;
}

type immediate = t

module T0 = struct
  type nonrec t = t

  let compare t1 t2 = TO.compare t1.value t2.value

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash t = TO.hash t.value

  let print ppf t =
    let print_as_char =
      t.print_as_char
        && TO.compare t.value TO.zero >= 0
        && TO.compare t.value TO.hex_ff <= 0
    in
    if print_as_char then
      Format.fprintf ppf "%C"
        (Char.chr (TO.bottom_byte_to_int t.value))
    else
      Format.fprintf ppf "%a"
        TO.print t.value
(*
    if print_as_char then
      Format.fprintf ppf "(immediate '%c')"
        (Char.chr (TO.bottom_byte_to_int t.value))
    else
      Format.fprintf ppf "(immediate %a)"
        TO.print t.value
*)
  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end

module Self = Identifiable.Make (T0)

include Self

module Pair = struct
  include Identifiable.Make_pair (struct
    type nonrec t = t
    include Self
  end) (struct
    type nonrec t = t
    include Self
  end)

  type nonrec t = t * t
end

let cross_product = Pair.create_from_cross_product

let join t1 t2 : t or_wrong =
  if not (TO.equal t1.value t2.value) then
    Wrong
  else
    let print_as_char = t1.print_as_char && t2.print_as_char in
    Ok {
      value = t1.value;
      print_as_char;
    }

let join_set t1s t2s =
  let only_in_t2s = Set.diff t2s t1s in
  let join =
    Set.fold (fun t1 result ->
        match Set.find t1 t2s with
        | exception Not_found -> Set.add t1 result
        | t2 ->
          match join t1 t2 with
          | Wrong -> result
          | Ok t -> Set.add t result)
      t1s
      Set.empty
  in
  Set.union join only_in_t2s

let bool_true = {
  value = TO.one;
  print_as_char = false;
}

let bool_false = {
  value = TO.zero;
  print_as_char = false;
}

let bool b =
  if b then bool_true else bool_false

let int value = {
  value;
  print_as_char = false;
}

let char value = {
  value = TO.of_char value;
  print_as_char = true;
}

let tag tag = int (TO.of_int (Tag.to_int tag))

let to_targetint t = t.value
let to_targetint' t = Targetint.OCaml.to_targetint t.value

let to_tag t = Tag.create_from_targetint (to_targetint t)

let map t ~f =
  { value = f t.value;
    print_as_char = t.print_as_char;
  }

let is_non_negative t =
  Targetint.OCaml.compare t.value Targetint.OCaml.zero >= 0

let set_to_targetint_set (set : Set.t) : TO.Set.t =
  Set.fold (fun t targetints -> TO.Set.add t.value targetints)
    set
    TO.Set.empty

let set_to_targetint_set' (set : Set.t) : Targetint.Set.t =
  Set.fold (fun t targetints ->
      Targetint.Set.add (TO.to_targetint t.value) targetints)
    set
    Targetint.Set.empty

let set_of_targetint_set (tis : TO.Set.t) : Set.t =
  TO.Set.fold (fun i set -> Set.add (int i) set)
    tis
    Set.empty

let set_of_targetint_set' (tis : Targetint.Set.t) : Set.t =
  Targetint.Set.fold (fun i set ->
      Set.add (int (TO.of_targetint i)) set)
    tis
    Set.empty

let all_bools =
  Set.of_list [bool_true; bool_false]

let map_value1 f t =
  { t with
    value = f t.value;
  }

let map_value2 f t0 t1 =
  { value = f t0.value t1.value;
    print_as_char = t0.print_as_char && t1.print_as_char;
  }

let map_value2' f t i =
  { t with
    value = f t.value i;
  }

let get_least_significant_16_bits_then_byte_swap
  = map_value1 TO.get_least_significant_16_bits_then_byte_swap
let shift_right_logical = map_value2' TO.shift_right_logical
let shift_right = map_value2' TO.shift_right
let shift_left = map_value2' TO.shift_left
let xor = map_value2 TO.xor
let or_ = map_value2 TO.or_
let and_ = map_value2 TO.and_
let mod_ = map_value2 TO.mod_
let div = map_value2 TO.div
let mul = map_value2 TO.mul
let sub = map_value2 TO.sub
let add = map_value2 TO.add
let neg = map_value1 TO.neg

let minus_one = int TO.minus_one
let zero = int TO.zero
let one = int TO.one

module Or_unknown = struct
  type nonrec t =
    | Ok of t
    | Unknown

  let ok imm = Ok imm
  let unknown () = Unknown

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Ok _, Unknown -> -1
      | Unknown, Ok _ -> 1
      | Unknown, Unknown -> 0
      | Ok imm1, Ok imm2 -> compare imm1 imm2

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash t =
      match t with
      | Ok imm -> Hashtbl.hash (0, hash imm)
      | Unknown -> Hashtbl.hash 1

    let print ppf t =
      match t with
      | Ok imm -> print ppf imm
      | Unknown -> Format.pp_print_string ppf "Unknown"

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)
end
