(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

module Bound_symbols = Let_symbol.Bound_symbols
module Field_of_block = Static_const.Field_of_block

(* CR-someday mshinwell: Finish improved simplification using types *)

let simplify_field_of_block dacc (field : Field_of_block.t) =
  match field with
  | Symbol sym -> field, T.alias_type_of K.value (Simple.symbol sym)
  | Tagged_immediate i -> field, T.this_tagged_immediate i
  | Dynamically_computed var ->
    let min_name_mode = Name_mode.normal in
    match S.simplify_simple dacc (Simple.var var) ~min_name_mode with
    | Bottom, ty ->
      assert (K.equal (T.kind ty) K.value);
      (* CR mshinwell: This should be "invalid" and propagate up *)
      field, T.bottom K.value
    | Ok simple, ty ->
      Simple.pattern_match simple
        ~name:(fun name ->
          Name.pattern_match name
            ~var:(fun var -> Field_of_block.Dynamically_computed var, ty)
            ~symbol:(fun sym -> Field_of_block.Symbol sym, ty))
        ~const:(fun const ->
          match Reg_width_const.descr const with
          | Tagged_immediate imm -> Field_of_block.Tagged_immediate imm, ty
          | Naked_immediate _ | Naked_float _ | Naked_int32 _
              | Naked_int64 _ | Naked_nativeint _ ->
            (* CR mshinwell: This should be "invalid" and propagate up *)
            field, ty)

let simplify_or_variable dacc type_for_const
      (or_variable : _ Or_variable.t) =
  let denv = DA.denv dacc in
  match or_variable with
  | Const const -> or_variable, type_for_const const
  | Var var ->
    (* CR mshinwell: This needs to check the type of the variable according
       to the various cases below. *)
    (* CR mshinwell: This should be calling [simplify_simple] *)
    or_variable, DE.find_variable denv var

let simplify_static_const_of_kind_value dacc
      (static_const : Static_const.t) ~result_sym
      : Static_const.t * DA.t =
  let bind_result_sym typ =
    DA.map_denv dacc ~f:(fun denv ->
      let denv =
        (* [result_sym] will already be defined when we are lifting
           reified continuation parameters. *)
        (* CR mshinwell: This is kind of nasty---try to rearrange things
           so this doesn't happen. *)
        DE.define_symbol_if_undefined denv result_sym K.value
      in
      DE.add_equation_on_symbol denv result_sym typ)
  in
  match static_const with
  | Block (tag, is_mutable, fields) ->
    let fields_with_tys =
      List.map (fun field -> simplify_field_of_block dacc field) fields
    in
    let fields, field_tys = List.split fields_with_tys in
    let ty =
      (* Same as Simplify_variadic_primitive.simplify_make_block_of_values *)
      let tag = Tag.Scannable.to_tag tag in
      let fields = field_tys in
      match is_mutable with
      | Immutable ->
        T.immutable_block ~is_unique:false tag ~field_kind:K.value ~fields
      | Immutable_unique ->
        T.immutable_block ~is_unique:true tag ~field_kind:K.value ~fields
      | Mutable -> T.any_value ()
    in
    let dacc = bind_result_sym ty in
    Block (tag, is_mutable, fields), dacc
  (* CR mshinwell: Need to reify to change Equals types into new terms *)
  | Boxed_float or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_float f) or_var
    in
    let dacc = bind_result_sym ty in
    Boxed_float or_var, dacc
  | Boxed_int32 or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_int32 f) or_var
    in
    let dacc = bind_result_sym ty in
    Boxed_int32 or_var, dacc
  | Boxed_int64 or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_int64 f) or_var
    in
    let dacc = bind_result_sym ty in
    Boxed_int64 or_var, dacc
  | Boxed_nativeint or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_nativeint f) or_var
    in
    let dacc = bind_result_sym ty in
    Boxed_nativeint or_var, dacc
  | Immutable_float_block fields ->
    let fields_with_tys =
      List.map (fun field ->
          simplify_or_variable dacc (fun f -> T.this_naked_float f) field)
        fields
    in
    let fields, _field_tys = List.split fields_with_tys in
    let dacc = bind_result_sym (T.any_value ()) in
    Immutable_float_block fields, dacc
  | Immutable_float_array fields ->
    let fields_with_tys =
      List.map (fun field ->
          simplify_or_variable dacc (fun f -> T.this_naked_float f) field)
        fields
    in
    let fields, _field_tys = List.split fields_with_tys in
    let dacc = bind_result_sym (T.any_value ()) in
    Immutable_float_array fields, dacc
  | Mutable_string { initial_value; } ->
    let str_ty = T.mutable_string ~size:(String.length initial_value) in
    let static_const : Static_const.t =
      Mutable_string {
        initial_value;
      }
    in
    let dacc = bind_result_sym str_ty in
    static_const, dacc
  | Immutable_string str ->
    let ty = T.this_immutable_string str in
    let dacc = bind_result_sym ty in
    Immutable_string str, dacc
  | Sets_of_closures _ ->
    Misc.fatal_errorf "[Sets_of_closures] cannot be bound by a \
        [Singleton] binding:@ %a"
      SC.print static_const

let simplify_static_const dacc (bound_symbols : Bound_symbols.t)
      (static_const : SC.t) : Bound_symbols.t * SC.t * DA.t =
  match bound_symbols with
  | Singleton result_sym ->
    let static_const, dacc =
      simplify_static_const_of_kind_value dacc static_const ~result_sym
    in
    bound_symbols, static_const, dacc
  | Sets_of_closures bound_symbols' ->
    let sets = SC.must_be_sets_of_closures static_const in
    Simplify_set_of_closures.simplify_lifted_sets_of_closures dacc
      ~orig_bound_symbols:bound_symbols ~orig_static_const:static_const
      bound_symbols' sets
