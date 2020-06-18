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

module RWC = Reg_width_const

include Reg_width_things.Simple

let const_bool b = const (if b then RWC.const_true else RWC.const_false)

let const_true = const_bool true
let const_false = const_bool false

let untagged_const_true = const RWC.untagged_const_true
let untagged_const_false = const RWC.untagged_const_false

let untagged_const_zero = const RWC.untagged_const_zero

let untagged_const_int i = const (RWC.untagged_const_int i)

let const_int i = const (RWC.const_int i)
let const_zero = const RWC.const_zero
let const_one = const RWC.const_one

let const_unit = const RWC.const_unit

let [@inline always] is_var t =
  pattern_match t ~name:Name.is_var ~const:(fun _ -> false)

let [@inline always] is_symbol t =
  pattern_match t ~name:Name.is_symbol ~const:(fun _ -> false)

let [@inline always] is_const t =
  pattern_match t ~name:(fun _ -> false) ~const:(fun _ -> true)

let const_from_descr descr = const (RWC.of_descr descr)

let without_rec_info t = pattern_match t ~name ~const

let merge_rec_info t ~newer_rec_info =
  if is_const t then None
  else
    match newer_rec_info with
    | None -> Some t
    | Some newer_rec_info ->
      let rec_info =
        match rec_info t with
        | None -> newer_rec_info
        | Some older_rec_info ->
          Rec_info.merge older_rec_info ~newer:newer_rec_info
      in
      Some (with_rec_info (without_rec_info t) rec_info)

(* CR mshinwell: Make naming consistent with [Name] re. the option type *)

(* CR mshinwell: Careful that Rec_info doesn't get dropped using the
   following *)

let [@inline always] must_be_var t =
  pattern_match t ~name:Name.must_be_var_opt ~const:(fun _ -> None)

let [@inline always] must_be_symbol t =
  pattern_match t ~name:Name.must_be_symbol_opt ~const:(fun _ -> None)

let [@inline always] must_be_name t =
  pattern_match t ~name:(fun name -> Some name) ~const:(fun _ -> None)

let to_name t =
  match must_be_name t with
  | None -> None
  | Some name -> Some (rec_info t, name)

let map_name t ~f =
  match must_be_name t with
  | None -> t
  | Some old_name -> name (f old_name)

let map_var t ~f =
  match must_be_name t with
  | None -> t
  | Some old_name -> name (Name.map_var old_name ~f)

let map_symbol t ~f =
  match must_be_name t with
  | None -> t
  | Some old_name -> name (Name.map_symbol old_name ~f)

let free_names t =
  pattern_match t
    ~name:(fun name -> Name_occurrences.singleton_name name Name_mode.normal)
    ~const:(fun _ -> Name_occurrences.empty)

let free_names_in_types t =
  pattern_match t
    ~name:(fun name -> Name_occurrences.singleton_name name Name_mode.in_types)
    ~const:(fun _ -> Name_occurrences.empty)

let apply_name_permutation t perm =
  let [@inline always] name old_name =
    let new_name = Name_permutation.apply_name perm old_name in
    if old_name == new_name then t
    else
      match rec_info t with
      | None -> name new_name
      | Some rec_info -> with_rec_info (name new_name) rec_info
  in
  pattern_match t ~const:(fun _ -> t) ~name

module List = struct
  type nonrec t = t list

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      Misc.Stdlib.List.compare compare t1 t2

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash t =
      Hashtbl.hash (List.map hash t)

    let print ppf t =
      (Format.pp_print_list print ~pp_sep:Format.pp_print_space) ppf t

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  let free_names t =
    List.fold_left (fun free t ->
        Name_occurrences.union free (free_names t))
      (Name_occurrences.empty)
      t

  let apply_name_permutation t perm =
    let changed = ref false in
    let result =
      List.map (fun simple ->
          let simple' = apply_name_permutation simple perm in
          if not (simple == simple') then begin
            changed := true
          end;
          simple')
        t
    in
    if not !changed then t
    else result
end

module Pair = struct
  include Identifiable.Make_pair
    (Reg_width_things.Simple)
    (Reg_width_things.Simple)

  type nonrec t = t * t
end

module With_kind = struct
  type nonrec t = t * Flambda_kind.t

  include Identifiable.Make (struct
    type nonrec t = t

    let compare (s1, k1) (s2, k2) =
      let c = compare s1 s2 in
      if c <> 0 then c
      else Flambda_kind.compare k1 k2

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash (s, k) =
      Hashtbl.hash (hash s, Flambda_kind.hash k)

    let print ppf (s, k) =
      Format.fprintf ppf "@[(%a@ @<1>\u{2237}@ %a)@]"
        print s
        Flambda_kind.print k

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  let free_names (simple, _kind) = free_names simple

  let apply_name_permutation ((simple, kind) as t) perm =
    let simple' = apply_name_permutation simple perm in
    if simple == simple' then t
    else simple', kind
end
