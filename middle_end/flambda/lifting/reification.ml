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
(*   special exception on linking described in the file LICENSDE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

let create_static_const (to_lift : T.to_lift) : Static_const.t =
  match to_lift with
  | Immutable_block { tag; is_unique; fields; } ->
    let of_kind_values =
      List.map (fun (field : T.var_or_symbol_or_tagged_immediate)
              : Static_const.Field_of_block.t ->
          match field with
          | Var var -> Dynamically_computed var
          | Symbol sym -> Symbol sym
          | Tagged_immediate imm -> Tagged_immediate imm)
        fields
    in
    let mut : Mutability.t =
      if is_unique then Immutable_unique else Immutable
    in
    Block (tag, mut, of_kind_values)
  | Boxed_float f -> Boxed_float (Const f)
  | Boxed_int32 i -> Boxed_int32 (Const i)
  | Boxed_int64 i -> Boxed_int64 (Const i)
  | Boxed_nativeint i -> Boxed_nativeint (Const i)

let lift dacc ty ~bound_to static_const =
  let dacc, symbol =
    match DA.find_shareable_constant dacc static_const with
    | Some symbol ->
      if !Clflags.flambda_invariant_checks
        && not (DE.mem_symbol (DA.denv dacc) symbol)
      then begin
        Misc.fatal_errorf "Constant with symbol %a is shareable but not in \
            the environment:@ %a"
          Symbol.print symbol
          DA.print dacc
      end;
      dacc, symbol
    | None ->
      let symbol =
        Symbol.create (Compilation_unit.get_current_exn ())
          (Linkage_name.create (Variable.unique_name bound_to))
      in
      if not (K.equal (T.kind ty) K.value) then begin
        Misc.fatal_errorf "Cannot lift non-[Value] variable: %a"
          Variable.print bound_to
      end;
      let dacc =
        let denv = DA.denv dacc in
        Lifted_constant.create_singleton symbol static_const denv ty
        |> DA.add_lifted_constant dacc
      in
      let dacc =
        DA.consider_constant_for_sharing dacc symbol static_const
        |> DA.map_denv ~f:(fun denv -> DE.add_symbol denv symbol ty)
      in
      dacc, symbol
  in
  let symbol' = Simple.symbol symbol in
  let term = Named.create_simple symbol' in
  let var_ty = T.alias_type_of (T.kind ty) symbol' in
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
      DE.add_equation_on_variable denv bound_to var_ty)
  in
  Reachable.reachable term, dacc, var_ty

let try_to_reify dacc (term : Reachable.t) ~bound_to =
  let occ_kind = Var_in_binding_pos.name_mode bound_to in
  let bound_to = Var_in_binding_pos.var bound_to in
  let denv = DA.denv dacc in
  let ty = DE.find_variable denv bound_to in
  match term with
  | Invalid _ ->
    let ty = T.bottom_like ty in
    let denv = DE.add_equation_on_variable denv bound_to ty in
    Reachable.invalid (), DA.with_denv dacc denv, ty
  | Reachable _ ->
    match T.reify (DE.typing_env denv) ~min_name_mode:occ_kind ty with
    | Lift to_lift ->
      if Name_mode.is_normal occ_kind then
        let static_const = create_static_const to_lift in
        (* We cannot currently handle the lifting of constants that are
           recursive with a symbol currently being defined, unless the
           constant is a closure, which it never is in this case.
           (An example of such a constant would be a pair, created in a
           recursive function [f] that has no free variables, containing
           the closure [f].) *)
        let overlap_with_current_definitions =
          let free_names = Static_const.free_names static_const in
          Symbol.Set.exists (fun sym ->
              Name_occurrences.mem_symbol free_names sym)
            (DE.symbols_currently_being_defined denv)
        in
        if overlap_with_current_definitions then term, dacc, ty
        else lift dacc ty ~bound_to static_const
      else
        term, dacc, ty
    | Simple simple ->
      (* CR mshinwell: Think about whether this is the best way of handling
         this. *)
      (* It is possible that the only [Simple] that [reify] could return is
         in fact [bound_to] -- for example when all other aliases are of
         an unsuitable occurrence kind. *)
      let dacc =
        if Simple.equal simple (Simple.var bound_to) then dacc
        else
          let ty = T.alias_type_of (T.kind ty) simple in
          let denv = DE.add_equation_on_variable denv bound_to ty in
          DA.with_denv dacc denv
      in
      if Simple.equal (Simple.var bound_to) simple then term, dacc, ty
      else Reachable.reachable (Named.create_simple simple), dacc, ty
    | Lift_set_of_closures _  (* already dealt with in [Simplify_named] *)
    | Cannot_reify -> term, dacc, ty
    | Invalid ->
      let ty = T.bottom_like ty in
      let denv = DE.add_equation_on_variable denv bound_to ty in
      Reachable.invalid (), DA.with_denv dacc denv, ty
