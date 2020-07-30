(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

(* CR mshinwell: This transformation is a nuisance as it entails
   complications to [Sort_lifted_constants] to get the inserted
   [Let_symbol] bindings in the correct order.  It isn't clear that it
   is necessary now that we can lift inconstants as a matter of course
   on the downwards traversal.
   vlaviron also thinks that this pass is overly sensitive to traversal order
   in some way
let lift_non_closure_discovered_via_reified_continuation_param_types dacc
      var to_lift ~reified_continuation_params_to_symbols
      ~reified_definitions ~closure_symbols_by_set =
  let static_const = Reification.create_static_const to_lift in
  match DA.find_shareable_constant dacc static_const with
  | Some symbol ->
    let reified_continuation_params_to_symbols =
      Variable.Map.add var symbol reified_continuation_params_to_symbols
    in
    dacc, reified_continuation_params_to_symbols, reified_definitions,
      closure_symbols_by_set
  | None ->
    let symbol =
      Symbol.create (Compilation_unit.get_current_exn ())
        (Linkage_name.create (Variable.unique_name var))
    in
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
        DE.add_equation_on_name (DE.define_symbol denv symbol K.value)
          (Name.var var)
          (T.alias_type_of K.value (Simple.symbol symbol)))
    in
    let dacc = DA.consider_constant_for_sharing dacc symbol static_const in
    let lifted_constant =
      (* The environment here may be used by [Sort_lifted_constants], but the
         type will not be. *)
      LC.create_block_like symbol static_const (DA.denv dacc) (T.any_value ())
    in
    let reified_definitions = (lifted_constant, None) :: reified_definitions in
    let reified_continuation_params_to_symbols =
      Variable.Map.add var symbol reified_continuation_params_to_symbols
    in
    dacc, reified_continuation_params_to_symbols, reified_definitions,
      closure_symbols_by_set

(* CR mshinwell: Can the following work on non-inlinable functions if we
   know enough about them? *)

let lift_set_of_closures_discovered_via_reified_continuation_param_types dacc
      var closure_id function_decls ~closure_vars
      ~reified_continuation_params_to_symbols
      ~reified_definitions ~closure_symbols_by_set =
  let module I = T.Function_declaration_type.Inlinable in
  let set_of_closures =
    let function_decls =
      Closure_id.Lmap.map (fun inlinable ->
        Function_declaration.create ~code_id:(I.code_id inlinable)
          ~params_arity:(I.param_arity inlinable)
          ~result_arity:(I.result_arity inlinable)
          ~stub:(I.stub inlinable)
          ~dbg:(I.dbg inlinable)
          ~inline:(I.inline inlinable)
          ~is_a_functor:(I.is_a_functor inlinable)
          ~recursive:(I.recursive inlinable)
          ~is_tupled:(I.is_tupled inlinable))
        function_decls
      |> Function_declarations.create
    in
    Set_of_closures.create function_decls ~closure_elements:closure_vars
  in
  let bind_continuation_param_to_symbol dacc ~closure_symbol_map =
    let dacc, symbol =
      DA.map_denv2 dacc ~f:(fun denv ->
        match Closure_id.Map.find closure_id closure_symbol_map with
        | exception Not_found ->
          Misc.fatal_errorf "Variable %a claimed to hold closure with \
              closure ID %a, but no symbol was found for that closure ID"
            Variable.print var
            Closure_id.print closure_id
        | symbol ->
          let denv =
            DE.add_equation_on_name denv (Name.var var)
              (T.alias_type_of K.value (Simple.symbol symbol))
          in
          denv, symbol)
    in
    dacc, Variable.Map.add var symbol reified_continuation_params_to_symbols
  in
  match Set_of_closures.Map.find set_of_closures closure_symbols_by_set with
  | exception Not_found ->
    let closure_symbols =
      Closure_id.Lmap.mapi (fun closure_id _function_decl ->
          (* CR mshinwell: share name computation with
             [Simplify_named] *)
          let name =
            closure_id
            |> Closure_id.rename
            |> Closure_id.to_string
            |> Linkage_name.create
          in
          Symbol.create (Compilation_unit.get_current_exn ()) name)
        function_decls
    in
    let dacc =
      DA.map_denv dacc ~f:(fun denv ->
        Closure_id.Lmap.fold (fun _closure_id closure_symbol denv ->
            DE.define_symbol denv closure_symbol K.value)
          closure_symbols
          denv)
    in
    (* This reification process will result in [Let_symbol] bindings containing
       closure symbol definitions but no code.  The code will be simplified
       when each such [Let_symbol] binding (occurring around the handler of the
       continuation whose parameters are being reified) is simplified.  This
       process of simplification, by virtue of the new equalities to symbols
       that we add into the environment, can reveal new dependencies between
       the [Let_symbol] bindings.  This can invalidate the order of the bindings
       even if such order was correct before the simplification.
       As a result, we make sure that any hidden dependencies are revealed and
       exposed to the sorting algorithm for constants (called below), with two
       separate aims:
       1. To ensure that the order of the [Let_symbol] bindings is correct in
          terms of symbol scope.
       2. To ensure that typing information flows in the expected manner.
          We define symbols in the environment early for this transformation
          (see above), so that we can introduce the equalities to the relevant
          variables; as such, a reference to such a symbol that is "too early"
          can just yield "unknown" rather than the proper type of the symbol.
          This should be avoided.
       Since the existing code in the [Let_symbol] bindings that we create below
       cannot contain any of the new closure symbols---and code is closed
       apart from symbols---the only thing we need to look at is the contents of
       the environment of each closure.  This needs to be done transitively as
       inlining may occur.  The aim is to discover variables that might be
       equated to symbols and to explicitly expose them as dependencies. *)
    let extra_deps =
      Var_within_closure.Map.fold (fun _var simple env_free_names ->
          let ty =
            Simple.pattern_match simple
              ~const:(fun const -> T.type_for_const const)
              ~name:(fun name -> TE.find (DA.typing_env dacc) name None)
          in
          Name_occurrences.union env_free_names
            (TE.free_names_transitive (DA.typing_env dacc) ty))
        closure_vars
        Name_occurrences.empty
    in
    let closure_symbols_with_types =
      (* The environments in this mapping may be used by
         [Sort_lifted_constants], but the types will not be. *)
      Closure_id.Lmap.map (fun closure_symbol -> closure_symbol, T.any_value ())
        closure_symbols
    in
    let lifted_constant =
      (* We don't need to assign new code IDs, since we're not changing the
         code. The code will actually be re-simplified (when we reach the new
         [Let_symbol] bindings)---at that point, new code IDs may well be
         assigned. (That is also the point at which references to the closures
         being lifted, via the continuation's parameters, will be changed to go
         via symbols.)  See long comment above concerning subtle point
         relating to dependencies that might be exposed during such
         simplification. *)
      LC.create_set_of_closures (DA.denv dacc)
        ~closure_symbols_with_types
        (Set_of_closures set_of_closures)
    in
    let reified_definitions =
      (lifted_constant, Some (DA.denv dacc, extra_deps)) :: reified_definitions
    in
    let closure_symbol_map =
      Closure_id.Lmap.bindings closure_symbols
      |> Closure_id.Map.of_list
    in
    let closure_symbols_by_set =
      Set_of_closures.Map.add set_of_closures closure_symbol_map
        closure_symbols_by_set
    in
    let dacc, reified_continuation_params_to_symbols =
      bind_continuation_param_to_symbol dacc ~closure_symbol_map
    in
    dacc, reified_continuation_params_to_symbols, reified_definitions,
      closure_symbols_by_set
  | closure_symbol_map ->
    let dacc, reified_continuation_params_to_symbols =
      bind_continuation_param_to_symbol dacc ~closure_symbol_map
    in
    dacc, reified_continuation_params_to_symbols, reified_definitions,
      closure_symbols_by_set

let reify_types_of_continuation_param_types dacc ~params =
  let orig_typing_env = DA.typing_env dacc in
  let disallowed_free_vars = KP.List.var_set params in
  List.fold_left
    (fun (dacc, reified_continuation_params_to_symbols, reified_definitions,
          closure_symbols_by_set) param ->
      let var = KP.var param in
      let kind = KP.kind param in
      let ty = TE.find orig_typing_env (Name.var var) (Some kind) in
      let existing_symbol =
        (* We must avoid attempting to create aliases between symbols or
           (equivalently) defining static parts that already have symbols.
           This could happen if [var] is actually equal to another of the
           continuation's parameters. *)
        match
          TE.get_canonical_simple_exn (DA.typing_env dacc)
            ~min_name_mode:NM.normal (Simple.var var)
        with
        | exception Not_found -> None
        | simple -> Simple.must_be_symbol simple
      in
      match existing_symbol with
      | Some _ ->
        dacc, reified_continuation_params_to_symbols, reified_definitions,
          closure_symbols_by_set
      | None ->
        (* CR mshinwell: I'm not sure the following statement is true any
           more, but I don't think we want to allow the [params] to appear
           anyway, as it may cause the allocation we're trying to remove not
           to go away.  (Try on mlexamples/lifting.ml for example.) *)
        (* Since the continuation we're dealing with might be inlined and
           we don't handle [extra_params_and_args] on such continuations at
           the [Apply_cont] site, be certain that the only variables appearing
           in our new [Let_symbol] bindings don't involve any of those
           (extra) parameters. *)
        let typing_env = DA.typing_env dacc in
        match
          T.reify ~allowed_if_free_vars_defined_in:typing_env
            ~disallowed_free_vars typing_env ~min_name_mode:NM.normal ty
        with
        | Lift to_lift ->
          (* There's no point in lifting constant values, as these should
             already have been lifted. *)
          let inconstant =
            match to_lift with
            | Immutable_block { tag = _; is_unique = _; fields; } ->
              List.exists (fun (field : T.var_or_symbol_or_tagged_immediate) ->
                  match field with
                  | Var _ -> true
                  | Symbol _ | Tagged_immediate _ -> false)
                fields
            | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
            | Boxed_nativeint _ -> false
          in
          if not inconstant then
            dacc, reified_continuation_params_to_symbols, reified_definitions,
              closure_symbols_by_set
          else
            lift_non_closure_discovered_via_reified_continuation_param_types
              dacc var to_lift ~reified_continuation_params_to_symbols
              ~reified_definitions ~closure_symbols_by_set
        | Lift_set_of_closures { closure_id; function_decls; closure_vars; } ->
          (* Same comment as above, specifically relating to the
             environment in this case. *)
          let inconstant_env =
            Var_within_closure.Map.exists (fun _ simple -> Simple.is_var simple)
              closure_vars
          in
          if not inconstant_env then
            dacc, reified_continuation_params_to_symbols, reified_definitions,
              closure_symbols_by_set
          else
            (* CR lmaurer: function_decls has arbitrary order *)
            let function_decls =
              Closure_id.Map.bindings function_decls
              |> Closure_id.Lmap.of_list
            in
            lift_set_of_closures_discovered_via_reified_continuation_param_types
              dacc var closure_id function_decls ~closure_vars
              ~reified_continuation_params_to_symbols
              ~reified_definitions ~closure_symbols_by_set
        | Simple _ | Cannot_reify | Invalid ->
          dacc, reified_continuation_params_to_symbols, reified_definitions,
            closure_symbols_by_set)
    (dacc, Variable.Map.empty, [], Set_of_closures.Map.empty)
    params

let lift_via_reification_of_continuation_param_types0 dacc ~params
      ~(extra_params_and_args : Continuation_extra_params_and_args.t)
      ~(handler : Expr.t) =
(*  Format.eprintf "-------- REIFY ------------\ndacc = @ %a\n%!"
    DA.print dacc; *)
  let dacc, _reified_continuation_params_to_symbols, reified_definitions,
      _closure_symbols_by_set =
    let params = params @ extra_params_and_args.extra_params in
    reify_types_of_continuation_param_types dacc ~params
  in
  (* CR mshinwell: If recursion extends beyond that which can be handled by the
     set-of-closures cases, then we would need something like the "symbol
     placeholder" approach (variables that are substituted for the
     continuation's parameters, which are in turn substituted for symbols at the
     Cmm translation phase). (Any SCC class containing >1 set of closures is
     maybe a bug?) *)
  let reified_definitions =
    Sort_lifted_constants.sort
      ~fold_over_lifted_constants:(fun ~init ~f ->
        ListLabels.fold_left reified_definitions ~init ~f)
  in
  let handler =
    List.fold_left (fun handler lifted_constant ->
        let bound_symbols = LC.bound_symbols lifted_constant in
        let defining_exprs = LC.defining_exprs lifted_constant in
        Expr.create_let_symbol bound_symbols Dominator defining_exprs handler)
      handler
      reified_definitions.bindings_outermost_last
  in
  dacc, handler
*)
let lift_via_reification_of_continuation_param_types dacc ~params:_
      ~extra_params_and_args:_ ~handler =
  (*
  if Flambda_features.lift_inconstants () then
    lift_via_reification_of_continuation_param_types0 dacc ~params
      ~extra_params_and_args ~handler
  else
  *)
    dacc, handler

let matches_extension_constructor static_const =
  match (static_const : Static_const.t) with
  | Block (tag, mut, _fields) ->
    begin match mut with
    | Immutable_unique ->
      (* Only extension constructors, with an object tag, are supposed to be
         Immutable_unique *)
      assert (Tag.equal Tag.object_tag (Tag.Scannable.to_tag tag));
      true
    | Mutable | Immutable -> false
    end
  | (Code _ | Set_of_closures _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
    | Boxed_nativeint _ | Immutable_float_block _ | Immutable_float_array _
    | Mutable_string _ | Immutable_string _) -> false

let allowed_for_toplevel_lifting static_const =
  (* Always try to lift extension constructors, as they're not likely
     to create many extra equations and can often prevent sets of closures
     from being closed if not lifted. *)
  if Flambda_features.lift_toplevel_inconstants () then true
  else matches_extension_constructor static_const

type reify_primitive_at_toplevel_result =
  | Lift of {
    symbol : Symbol.t;
    static_const : Flambda.Static_const.t;
  }
  | Shared of Symbol.t
  | Cannot_reify

let reify_primitive_at_toplevel dacc bound_var ty =
  let typing_env = DA.typing_env dacc in
  (* CR mshinwell: We're reifying twice here (the other occurrence being from
     [Simplify_named].  We should probably combine this code with the code in
     [Reification] somehow to avoid this.  It should also mean we don't need
     the [is_fully_static] check below. *)
  match
    T.reify ~allowed_if_free_vars_defined_in:typing_env
      ~allow_unique:true
      typing_env ~min_name_mode:NM.normal ty
  with
  | Lift to_lift ->
    (* There's no point in lifting constant values, as these should
       already have been lifted. *)
    let static_const = Reification.create_static_const to_lift in
    if Static_const.is_fully_static static_const
      || not (allowed_for_toplevel_lifting static_const)
    then
      Cannot_reify
    else begin
      match DA.find_shareable_constant dacc static_const with
      | Some symbol -> Shared symbol
      | None ->
        let symbol =
          Symbol.create (Compilation_unit.get_current_exn ())
            (Linkage_name.create
               (Variable.unique_name (Var_in_binding_pos.var bound_var)))
        in
        Lift { symbol; static_const; }
    end
  | Lift_set_of_closures _ | Simple _ | Cannot_reify | Invalid ->
    Cannot_reify
