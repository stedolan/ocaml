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

type t =
  | Simple of Simple.t
  | Prim of Flambda_primitive.t * Debuginfo.t
  | Set_of_closures of Set_of_closures.t

let create_simple simple = Simple simple
let create_prim prim dbg = Prim (prim, dbg)
let create_set_of_closures set_of_closures = Set_of_closures set_of_closures

let print_with_cache ~cache ppf (t : t) =
  match t with
  | Simple simple -> Simple.print ppf simple
  | Prim (prim, dbg) ->
    fprintf ppf "@[<hov 1>(%a@<0>%s%a@<0>%s)@]"
      Flambda_primitive.print prim
      (Flambda_colours.debuginfo ())
      Debuginfo.print_or_elide dbg
      (Flambda_colours.normal ())
  | Set_of_closures set_of_closures ->
    Set_of_closures.print_with_cache ~cache ppf set_of_closures

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

(* CR mshinwell: It seems that the type [Flambda_primitive.result_kind]
   should move into [K], now it's used here. *)
let invariant_returning_kind env t : Flambda_primitive.result_kind =
  try
    let module E = Invariant_env in
    match t with
    | Simple simple ->
      Singleton (E.kind_of_simple env simple)
    | Set_of_closures set_of_closures ->
      Set_of_closures.invariant env set_of_closures;
      Singleton K.fabricated
    | Prim (prim, dbg) ->
      Flambda_primitive.invariant env prim;
      ignore (dbg : Debuginfo.t);
      Flambda_primitive.result_kind prim
  with Misc.Fatal_error ->
    Misc.fatal_errorf "(during invariant checks) Context is:@ %a" print t

let invariant env t =
  ignore ((invariant_returning_kind env t) : Flambda_primitive.result_kind)

let free_names t =
  match t with
  | Simple simple -> Simple.free_names simple
  | Prim (prim, _dbg) -> Flambda_primitive.free_names prim
  | Set_of_closures set -> Set_of_closures.free_names set

let apply_name_permutation t perm =
  match t with
  | Simple simple ->
    let simple' = Simple.apply_name_permutation simple perm in
    if simple == simple' then t
    else Simple simple'
  | Prim (prim, dbg) ->
    let prim' = Flambda_primitive.apply_name_permutation prim perm in
    if prim == prim' then t
    else Prim (prim', dbg)
  | Set_of_closures set ->
    let set' = Set_of_closures.apply_name_permutation set perm in
    if set == set' then t
    else Set_of_closures set'

let all_ids_for_export t =
  match t with
  | Simple simple -> Ids_for_export.from_simple simple
  | Prim (prim, _dbg) -> Flambda_primitive.all_ids_for_export prim
  | Set_of_closures set -> Set_of_closures.all_ids_for_export set

let import import_map t =
  match t with
  | Simple simple ->
    let simple = Ids_for_export.Import_map.simple import_map simple in
    Simple simple
  | Prim (prim, dbg) ->
    let prim = Flambda_primitive.import import_map prim in
    Prim (prim, dbg)
  | Set_of_closures set ->
    let set = Set_of_closures.import import_map set in
    Set_of_closures set

let box_value name (kind : Flambda_kind.t) dbg : Named.t * Flambda_kind.t =
  let simple = Simple.name name in
  match kind with
  | Value -> Simple simple, kind
  | Naked_number Naked_immediate ->
    Misc.fatal_error "Not yet supported"
  | Naked_number Naked_float ->
    Prim (Unary (Box_number Naked_float, simple), dbg), K.value
  | Naked_number Naked_int32 ->
    Prim (Unary (Box_number Naked_int32, simple), dbg), K.value
  | Naked_number Naked_int64 ->
    Prim (Unary (Box_number Naked_int64, simple), dbg), K.value
  | Naked_number Naked_nativeint ->
    Prim (Unary (Box_number Naked_nativeint, simple), dbg), K.value
  | Fabricated ->
    Misc.fatal_error "Cannot box values of [Fabricated] kind"

let unbox_value name (kind : Flambda_kind.t) dbg : Named.t * Flambda_kind.t =
  let simple = Simple.name name in
  match kind with
  | Value -> Simple simple, kind
  | Naked_number Naked_immediate ->
    Misc.fatal_error "Not yet supported"
  | Naked_number Naked_float ->
    Prim (Unary (Unbox_number Naked_float, simple), dbg), K.naked_float
  | Naked_number Naked_int32 ->
    Prim (Unary (Unbox_number Naked_int32, simple), dbg), K.naked_int32
  | Naked_number Naked_int64 ->
    Prim (Unary (Unbox_number Naked_int64, simple), dbg), K.naked_int64
  | Naked_number Naked_nativeint ->
    Prim (Unary (Unbox_number Naked_nativeint, simple), dbg),
      K.naked_nativeint
  | Fabricated ->
    Misc.fatal_error "Cannot box values of [Fabricated] kind"

let at_most_generative_effects (t : t) =
  match t with
  | Simple _ -> true
  | Prim (prim, _) -> Flambda_primitive.at_most_generative_effects prim
  | Set_of_closures _ -> true

let dummy_value (kind : K.t) : t =
  let simple =
    match kind with
    | Value -> Simple.const_zero
    | Naked_number Naked_immediate ->
      Simple.const (Reg_width_const.naked_immediate Target_imm.zero)
    | Naked_number Naked_float ->
      Simple.const (
        Reg_width_const.naked_float Numbers.Float_by_bit_pattern.zero)
    | Naked_number Naked_int32 ->
      Simple.const (Reg_width_const.naked_int32 Int32.zero)
    | Naked_number Naked_int64 ->
      Simple.const (Reg_width_const.naked_int64 Int64.zero)
    | Naked_number Naked_nativeint ->
      Simple.const (Reg_width_const.naked_nativeint Targetint.zero)
    | Fabricated -> Misc.fatal_error "[Fabricated] kind not expected here"
  in
  Simple simple

let is_set_of_closures t =
  match t with
  | Set_of_closures _ -> true
  | Simple _ | Prim _ -> false
