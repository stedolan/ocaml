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
      bound_symbol : Let_symbol.Bound_symbols.t;
      static_const : Static_const.t;
      dacc : Downwards_acc.t;
    }
  | Shared of { symbol : Symbol.t; kind : Flambda_kind.t; }

let bindings_result bindings_outermost_first dacc =
  Bindings { bindings_outermost_first; dacc; }

let simplify_named0 dacc ~(bound_vars : Bindable_let_bound.t)
      (named : Named.t) =
  match named with
  | Simple simple ->
    let bound_var = Bindable_let_bound.must_be_singleton bound_vars in
    let min_name_mode = Var_in_binding_pos.name_mode bound_var in
    begin match S.simplify_simple dacc simple ~min_name_mode with
    | Bottom, ty ->
      let dacc = DA.add_variable dacc bound_var (T.bottom (T.kind ty)) in
      let defining_expr = Reachable.invalid () in
      bindings_result [bound_vars, defining_expr] dacc
    | Ok new_simple, ty ->
      let dacc = DA.add_variable dacc bound_var ty in
      let defining_expr =
        if simple == new_simple then Reachable.reachable named
        else Reachable.reachable (Named.create_simple simple)
      in
      bindings_result [bound_vars, defining_expr] dacc
    end
  | Prim (prim, dbg) ->
    let bound_var = Bindable_let_bound.must_be_singleton bound_vars in
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
    let defining_expr, dacc, ty =
      Reification.try_to_reify dacc term ~bound_to:bound_var
    in
    let defining_expr =
      if T.is_bottom (DA.typing_env dacc) ty then Reachable.invalid ()
      else defining_expr
    in
    if DE.at_unit_toplevel (DA.denv dacc)
      && Name_mode.is_normal (Var_in_binding_pos.name_mode bound_var)
    then begin
      match
        Lift_inconstants.reify_primitive_at_toplevel dacc bound_var ty
      with
      | Cannot_reify -> bindings_result [bound_vars, defining_expr] dacc
      | Shared { symbol; } ->
        Shared { symbol; kind; }
      | Lift { dacc; symbol; static_const; } ->
        let named = Named.create_simple (Simple.symbol symbol) in
        Reified
          { definition = named;
            bound_symbol = Let_symbol.Bound_symbols.Singleton symbol;
            static_const;
            dacc;
          }
    end
    else
      bindings_result [bound_vars, defining_expr] dacc
  | Set_of_closures set_of_closures ->
    let bindings, dacc =
      Simplify_set_of_closures.simplify_non_lifted_set_of_closures dacc
        ~bound_vars set_of_closures
    in
    bindings_result bindings dacc

let simplify_named dacc ~bound_vars named =
  try
    simplify_named0 dacc ~bound_vars named
  with Misc.Fatal_error -> begin
    if !Clflags.flambda2_context_on_error then begin
      Format.eprintf "\n%sContext is:%s simplifying [Let] binding@ %a =@ %a@ \
          with downwards accumulator:@ %a\n"
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        Bindable_let_bound.print bound_vars
        Named.print named
        DA.print dacc
    end;
    raise Misc.Fatal_error
  end
