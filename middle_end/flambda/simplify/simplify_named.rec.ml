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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

type simplify_named_result =
  | Bindings of {
      bindings_outermost_first : (Bindable_let_bound.t * Reachable.t) list;
      dacc : Downwards_acc.t;
    }
  | Reified of {
      definition : Named.t;
      symbol : Symbol.t;
      static_const : Static_const.t;
    }
  | Shared of Symbol.t

let bindings_result bindings_outermost_first dacc =
  Bindings { bindings_outermost_first; dacc; }

let simplify_named0 dacc (bindable_let_bound : Bindable_let_bound.t)
      (named : Named.t) =
  match named with
  | Simple simple ->
    let bound_var = Bindable_let_bound.must_be_singleton bindable_let_bound in
    let min_name_mode = Var_in_binding_pos.name_mode bound_var in
    begin match S.simplify_simple dacc simple ~min_name_mode with
    | Bottom, ty ->
      let dacc = DA.add_variable dacc bound_var (T.bottom (T.kind ty)) in
      let defining_expr = Reachable.invalid () in
      bindings_result [bindable_let_bound, defining_expr] dacc
    | Ok new_simple, ty ->
      let dacc = DA.add_variable dacc bound_var ty in
      let defining_expr =
        if simple == new_simple then Reachable.reachable named
        else Reachable.reachable (Named.create_simple simple)
      in
      bindings_result [bindable_let_bound, defining_expr] dacc
    end
  | Prim (prim, dbg) ->
    let bound_var = Bindable_let_bound.must_be_singleton bindable_let_bound in
    let term, env_extension, dacc =
      Simplify_primitive.simplify_primitive dacc ~original_named:named
        prim dbg ~result_var:bound_var
    in
    let kind = P.result_kind' prim in
    let dacc =
      let dacc = DA.add_variable dacc bound_var (T.unknown kind) in
      DA.extend_typing_environment dacc env_extension
    in
    (* CR mshinwell: Add check along the lines of: types are unknown
       whenever [not (P.With_fixed_value.eligible prim)] holds. *)
    (* Primitives with generative effects correspond to allocations.
       Without this check, we could end up lifting definitions that have
       a type that looks like an allocation but that are instead a projection
       from a bigger structure. *)
    let allow_lifting = P.only_generative_effects prim in
    let defining_expr, dacc, ty =
      Reification.try_to_reify dacc term ~bound_to:bound_var ~allow_lifting
    in
    let defining_expr =
      if T.is_bottom (DA.typing_env dacc) ty then Reachable.invalid ()
      else defining_expr
    in
    if DE.at_unit_toplevel (DA.denv dacc)
      && allow_lifting
      && Name_mode.is_normal (Var_in_binding_pos.name_mode bound_var)
    then begin
      match
        Lift_inconstants.reify_primitive_at_toplevel dacc bound_var ty
      with
      | Cannot_reify ->
        bindings_result [bindable_let_bound, defining_expr] dacc
      | Shared symbol -> Shared symbol
      | Lift { symbol; static_const; } ->
        let named = Named.create_simple (Simple.symbol symbol) in
        Reified
          { definition = named;
            symbol;
            static_const;
          }
    end
    else
      bindings_result [bindable_let_bound, defining_expr] dacc
  | Set_of_closures set_of_closures ->
    let bindings, dacc =
      Simplify_set_of_closures.simplify_non_lifted_set_of_closures dacc
        bindable_let_bound set_of_closures
    in
    bindings_result bindings dacc
  | Static_consts static_consts ->
    let { Bindable_let_bound. bound_symbols; scoping_rule = _; } =
      Bindable_let_bound.must_be_symbols bindable_let_bound
    in
    if not (DE.at_unit_toplevel (DA.denv dacc)) then begin
      Misc.fatal_errorf "[Let] binding symbols is only allowed at the toplevel \
          of compilation units (not even at the toplevel of function \
          bodies):@ %a@ =@ %a"
        Bindable_let_bound.print bindable_let_bound
        Named.print named
    end;
    let non_closure_symbols_being_defined =
      Bound_symbols.non_closure_symbols_being_defined bound_symbols
    in
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
        Symbol.Set.fold (fun symbol denv ->
            DE.now_defining_symbol denv symbol)
          non_closure_symbols_being_defined
          denv)
    in
    let bound_symbols, static_consts, dacc =
      try
        Simplify_static_const.simplify_static_consts dacc bound_symbols
          static_consts
      with Misc.Fatal_error -> begin
        if !Clflags.flambda_context_on_error then begin
          Format.eprintf "\n%sContext is:%s simplifying [Let_symbol] binding \
                            of@ %a@ with downwards accumulator:@ %a\n"
            (Flambda_colours.error ())
            (Flambda_colours.normal ())
            Bound_symbols.print bound_symbols
            DA.print dacc
          end;
          raise Misc.Fatal_error
      end
    in
    (* CR mshinwell: Change to be run only when invariants are on, and use
      [Name_occurrences.iter] (to be written).
    Symbol.Set.iter (fun sym ->
        DE.check_symbol_is_bound (DA.denv dacc) sym)
      (Name_occurrences.symbols bound_symbols_free_names);
    Code_id.Set.iter (fun code_id ->
        DE.check_code_id_is_bound (DA.denv dacc) code_id)
      (Name_occurrences.code_ids bound_symbols_free_names);
    *)
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
        Symbol.Set.fold (fun symbol denv ->
            DE.no_longer_defining_symbol denv symbol)
          non_closure_symbols_being_defined
          denv)
    in
    let dacc =
      Static_const.Group.match_against_bound_symbols static_consts bound_symbols
        ~init:dacc
        ~code:(fun dacc _ _ -> dacc)
        ~set_of_closures:(fun dacc ~closure_symbols:_ _ -> dacc)
        ~block_like:(fun dacc symbol static_const ->
          DA.consider_constant_for_sharing dacc symbol static_const)
    in
    let lifted_constants =
      ListLabels.map2
        (Bound_symbols.to_list bound_symbols)
        (Static_const.Group.to_list static_consts)
        ~f:(fun (pat : Bound_symbols.Pattern.t) static_const ->
          match pat with
          | Block_like symbol ->
            let typ =
              TE.find (DA.typing_env dacc) (Name.symbol symbol) (Some K.value)
            in
            LC.create_block_like symbol static_const (DA.denv dacc) typ
          | Code code_id -> LC.create_code code_id static_const
          | Set_of_closures closure_symbols ->
            let closure_symbols_with_types =
              Closure_id.Lmap.map (fun symbol ->
                  let typ =
                    TE.find (DA.typing_env dacc) (Name.symbol symbol) (Some K.value)
                  in
                  symbol, typ)
                closure_symbols
            in
            LC.create_set_of_closures (DA.denv dacc)
              ~closure_symbols_with_types
              static_const)
    in
    let dacc = DA.add_lifted_constant dacc (LC.concat lifted_constants) in
    (* We don't need to return any bindings; [Simplify_expr.simplify_let]
       will create the "let symbol" binding when it sees the lifted
       constant. *)
    Bindings { bindings_outermost_first = []; dacc; }

let simplify_named dacc bindable_let_bound named =
  try
    simplify_named0 dacc bindable_let_bound named
  with Misc.Fatal_error -> begin
    if !Clflags.flambda_context_on_error then begin
      Format.eprintf "\n%sContext is:%s simplifying [Let] binding@ %a =@ %a@ \
          with downwards accumulator:@ %a\n"
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        Bindable_let_bound.print bindable_let_bound
        Named.print named
        DA.print dacc
    end;
    raise Misc.Fatal_error
  end
