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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

(* CR mshinwell: Need to simplify each [dbg] we come across. *)
(* CR mshinwell: Consider defunctionalising to remove the [k]. *)
(* CR mshinwell: May in any case be able to remove the polymorphic recursion. *)
(* CR mshinwell: See whether resolution of continuation aliases can be made
   more transparent (e.g. through [find_continuation]).  Tricky potentially in
   conjunction with the rewrites. *)

type 'a k = DA.t -> ('a * UA.t)

let rec simplify_let
  : 'a. DA.t -> Let.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc let_expr k ->
  let module L = Flambda.Let in
  let original_dacc = dacc in
  L.pattern_match let_expr ~f:(fun bindable_let_bound ~body ->
    let defining_expr = L.defining_expr let_expr in
    let place_lifted_constants_immediately =
      let is_dynamically_allocated_set_of_closures =
        Named.is_dynamically_allocated_set_of_closures defining_expr
      in
      let is_let_symbol = Named.is_static_consts defining_expr in
      (* Simplification of toplevel dynamically-allocated sets of closures can
         yield constants that must be placed immediately around the body rather
         than propagated upwards to the previous enclosing "let symbol".  That
         this is so follows from the fact that such constants may involve
         variables in their definition.  We cannot follow the pattern of the
         [Reified] case below as that would cause the bodies of the
         functions involved to be simplified a second time.
         When we actually have a [Let] that binds symbol(s), this is the
         behaviour in all cases. *)
      is_let_symbol
        || (is_dynamically_allocated_set_of_closures
              (* CR mshinwell: We need to fix the performance problem that
                 Vincent investigated relating to construction of closure
                 environments, then uncomment this.
              && !Clflags.Flambda.lift_toplevel_inconstants
              *)
              && DE.at_unit_toplevel (DA.denv dacc))
    in
    (* Remember then clear the lifted constants memory in [DA] so we can
       easily find out which constants are generated during simplification
       of the defining expression and the [body]. *)
    let dacc, prior_lifted_constants = DA.get_and_clear_lifted_constants dacc in
    (* Simplify the defining expression. *)
    match
      Simplify_named.simplify_named dacc bindable_let_bound
        (L.defining_expr let_expr)
    with
    | Bindings { bindings_outermost_first = bindings; dacc; } ->
      (* This is the normal case.
         First remember any lifted constants that were generated during the
         simplification of the defining expression.  Then add back in to [dacc]
         the [prior_lifted_constants] remembered above.  This results in the
         definitions and types for all these constants being available at a
         subsequent [Let_cont].  At such a point, [dacc] will be queried to
         retrieve all of the constants, which are then manually transferred
         into the computed [dacc] at the join point for subsequent
         simplification of the continuation handler(s).
         Note that no lifted constants are ever placed during the simplification
         of the defining expression.  (Not even in the case of a
         [Set_of_closures] binding, since "let symbol" is disallowed under a
         lambda.)
      *)
      let lifted_constants_from_defining_expr = DA.get_lifted_constants dacc in
      let dacc = DA.add_lifted_constants dacc prior_lifted_constants in
      (* Simplify the body of the let-expression and make the new [Let] bindings
         around the simplified body.  [Simplify_named] will already have
         prepared [dacc] with the necessary bindings for the simplification of
         the body. *)
      let body, user_data, uacc = simplify_expr dacc body k in
      (* The lifted constants present in [uacc] are the ones arising from
         the simplification of [body] which still have to be placed.  We
         augment these with any constants arising from the simplification of
         the defining expression.  Then we either place them all, or return
         them in [uacc] for an outer [Let]-binding to deal with. *)
      let lifted_constants_from_body =
        UA.lifted_constants_still_to_be_placed uacc
      in
      let no_constants_to_place =
        LCS.is_empty lifted_constants_from_defining_expr
          && LCS.is_empty lifted_constants_from_body
      in
      (* If there is nothing to place, avoid the closure allocations below. *)
      if no_constants_to_place || not place_lifted_constants_immediately
      then begin
        let uacc =
          UA.with_lifted_constants_still_to_be_placed uacc
            (LCS.union lifted_constants_from_defining_expr
              lifted_constants_from_body)
        in
        let body = Simplify_common.bind_let_bound ~bindings ~body in
        body, user_data, uacc
      end else begin
        let scoping_rule =
          (* If this is a "normal" let rather than a "let symbol", then we
             use [Dominator] scoping for any symbol bindings we place, as the
             types of the symbols may have been used out of syntactic scope.
          *)
          Option.value ~default:Symbol_scoping_rule.Dominator
            (Bindable_let_bound.let_symbol_scoping_rule bindable_let_bound)
        in
        let calculate_constants_to_place lifted_constants_to_place
              ~critical_deps ~to_float =
          let sorted_lifted_constants =
            Sort_lifted_constants.sort
              ~fold_over_lifted_constants:(LCS.fold lifted_constants_to_place)
          in
          (* If we are at a [Dominator]-scoped binding then we float up
             as many constants as we can whose definitions are fully static
             (i.e. do not involve variables) to the nearest enclosing
             [Syntactic]ally-scoped [Let]-binding.  This is done by peeling
             off the definitions starting at the outermost one.  We keep
             track of the "critical dependencies", which are those symbols
             that are definitely going to have their definitions placed at
             the current [Let]-binding, and any reference to which in another
             binding (even if fully static) will cause that binding to be
             placed too. *)
          (* CR-soon mshinwell: This won't be needed once we can remove
             [Dominator]-scoped bindings; every "let symbol" can then have
             [Dominator] scoping. *)
          match scoping_rule with
          | Syntactic ->
            let to_place = sorted_lifted_constants.bindings_outermost_last in
            to_place, to_float, critical_deps
          | Dominator ->
            let bindings_outermost_first =
              List.rev sorted_lifted_constants.bindings_outermost_last
            in
            ListLabels.fold_left bindings_outermost_first
              ~init:([], to_float, critical_deps)
              ~f:(fun (to_place, to_float, critical_deps) lifted_const ->
                let bound_symbols = LC.bound_symbols lifted_const in
                let defining_exprs = LC.defining_exprs lifted_const in
                if Static_const.Group.is_fully_static defining_exprs then
                  let must_place =
                    Name_occurrences.inter_domain_is_non_empty critical_deps
                      (Static_const.Group.free_names defining_exprs)
                  in
                  if not must_place then
                    to_place, LCS.add to_float lifted_const, critical_deps
                  else
                    let critical_deps =
                      Name_occurrences.union critical_deps
                        (Bound_symbols.free_names bound_symbols)
                    in
                    lifted_const :: to_place, to_float, critical_deps
                else
                  let critical_deps =
                    Name_occurrences.union critical_deps
                      (Bound_symbols.free_names bound_symbols)
                  in
                  lifted_const :: to_place, to_float, critical_deps)
        in
        (* We handle constants arising from the defining expression, which
           may be used in [bindings], separately from those arising from the
           [body], which may reference the [bindings]. *)
        let to_place_outermost_last_around_defining_expr, to_float,
            critical_deps =
          calculate_constants_to_place lifted_constants_from_defining_expr
            ~critical_deps:Name_occurrences.empty ~to_float:LCS.empty
        in
        let critical_deps =
          (* Make sure we don't move constants past the [bindings] if there
             is a dependency. *)
          ListLabels.fold_left bindings ~init:critical_deps
            ~f:(fun critical_deps (bound, _) ->
              Name_occurrences.union (Bindable_let_bound.free_names bound)
                critical_deps)
        in
        let to_place_outermost_last_around_body, to_float, _critical_deps =
          calculate_constants_to_place lifted_constants_from_body
            ~critical_deps ~to_float
        in
        (* Propagate constants that are to float upwards. *)
        let uacc = UA.with_lifted_constants_still_to_be_placed uacc to_float in
        (* Place constants whose definitions must go at the current [Let]. *)
        let place_constants uacc ~around ~outermost_last =
          ListLabels.fold_left outermost_last
            ~init:(around, uacc) ~f:(fun (body, uacc) lifted_const ->
              Simplify_common.create_let_symbol uacc scoping_rule
                (UA.code_age_relation uacc) lifted_const body)
        in
        let body, uacc =
          place_constants uacc ~around:body
            ~outermost_last:to_place_outermost_last_around_body
        in
        let body = Simplify_common.bind_let_bound ~bindings ~body in
        let body, uacc =
          place_constants uacc ~around:body
            ~outermost_last:to_place_outermost_last_around_defining_expr
        in
        body, user_data, uacc
      end
    | Reified { definition; symbol; static_const; } ->
      if place_lifted_constants_immediately then begin
        Misc.fatal_errorf "Did not expect [Simplify_named] to return \
            [Reified] (bound symbol %a)"
          Symbol.print symbol
      end;
      (* In this case, the defining expression was found to be constant,
         so we generate a "let symbol" binding.  The constant being bound
         has not yet been added to [dacc]; that will happen during
         simplification of the following built [let_symbol_expr]. *)
      let bound_symbols =
        Bound_symbols.singleton (Bound_symbols.Pattern.block_like symbol)
      in
      let let_symbol_expr =
        Expr.create_let_symbol bound_symbols Dominator
          (Static_const.Group.create [static_const])
          (Expr.create_pattern_let bindable_let_bound definition body)
      in
      (* We're effectively replacing one expression (the original [Let])
         with another here; the new expression will be completely
         re-simplified.  So we don't have to do anything relating to lifted
         constants.
         We have to revert to [original_dacc] as types in [dacc] may have
         been affected (due to aliases) in ways that would be wrong to
         propagate.  We do however augment [original_dacc] so that it knows
         [symbol], which we have freshly generated, is eligible for
         sharing. *)
      let dacc =
        DA.consider_constant_for_sharing original_dacc symbol static_const
      in
      simplify_expr dacc let_symbol_expr k
    | Shared symbol ->
      if place_lifted_constants_immediately then begin
        Misc.fatal_errorf "Did not expect [Simplify_named] to return \
            [Shared] (symbol %a)"
          Symbol.print symbol
      end;
      (* This is similar to the [Reified] case above, except that the
         constant in question is structurally identical to one that has been
         generated previously, so we simply point the let-binding at the
         existing symbol. *)
      let let_expr =
        Expr.create_pattern_let bindable_let_bound
          (Named.create_simple (Simple.symbol symbol))
          body
      in
      (* Same comment as for the [Reified] case with regard to lifted
         constants and [original_dacc]. *)
      simplify_expr original_dacc let_expr k)

and simplify_one_continuation_handler :
 'a. DA.t
  -> Continuation.t
  -> Recursive.t
  -> CH.t
  -> params:KP.t list
  -> handler:Expr.t
  -> extra_params_and_args:Continuation_extra_params_and_args.t
  -> 'a k
  -> Continuation_handler.t * 'a * UA.t
= fun dacc cont (recursive : Recursive.t) (cont_handler : CH.t) ~params
      ~(handler : Expr.t) ~(extra_params_and_args : EPA.t) k ->
  (*
Format.eprintf "handler:@.%a@."
  Expr.print cont_handler.handler;
  *)
  (*
Format.eprintf "About to simplify handler %a, params %a, EPA %a\n%!"
  Continuation.print cont
  KP.List.print params
  EPA.print extra_params_and_args;
  *)
  let handler, user_data, uacc = simplify_expr dacc handler k in
  let handler, uacc =
  (*
    let () =
      Format.eprintf "For %a: simplified handler: %a\n%!"
        Continuation.print cont Expr.print handler
    in
    *)
    let free_names = Expr.free_names handler in
    let used_params =
      (* Removal of unused parameters of recursive continuations is not
         currently supported. *)
      match recursive with
      | Recursive -> params
      | Non_recursive ->
        let first = ref true in
        List.filter (fun param ->
            (* CR mshinwell: We should have a robust means of propagating which
               parameter is the exception bucket.  Then this hack can be
               removed. *)
            if !first && Continuation.is_exn cont then begin
              first := false;
              true
            end else begin
              first := false;
              Name_occurrences.mem_var free_names (KP.var param)
            end)
          params
    in
    let used_extra_params =
      List.filter (fun extra_param ->
          Name_occurrences.mem_var free_names (KP.var extra_param))
        extra_params_and_args.extra_params
    in
    (*
    let () =
      Format.eprintf "For %a: free names %a, \
          used_params %a, EP %a, used_extra_params %a\n%!"
        Continuation.print cont
        Name_occurrences.print free_names
        KP.List.print used_params
        KP.List.print extra_params_and_args.extra_params
        KP.List.print used_extra_params
    in
    *)
    let handler =
      let params = used_params @ used_extra_params in
      CH.with_params_and_handler cont_handler (CPH.create params ~handler)
    in
    let rewrite =
      Apply_cont_rewrite.create ~original_params:params
        ~used_params:(KP.Set.of_list used_params)
        ~extra_params:extra_params_and_args.extra_params
        ~extra_args:extra_params_and_args.extra_args
        ~used_extra_params:(KP.Set.of_list used_extra_params)
    in
    (*
Format.eprintf "Rewrite:@ %a\n%!" Apply_cont_rewrite.print rewrite;
*)
    let uacc =
      UA.map_uenv uacc ~f:(fun uenv ->
        UE.add_apply_cont_rewrite uenv cont rewrite)
    in
(*
Format.eprintf "Finished simplifying handler %a\n%!"
Continuation.print cont;
*)
    handler, uacc
  in
  handler, user_data, uacc

and simplify_non_recursive_let_cont_handler
  : 'a. DA.t -> Non_recursive_let_cont_handler.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc non_rec_handler k ->
  let cont_handler = Non_recursive_let_cont_handler.handler non_rec_handler in
  Non_recursive_let_cont_handler.pattern_match non_rec_handler
    ~f:(fun cont ~body ->
      let denv = DA.denv dacc in
      let unit_toplevel_exn_cont = DE.unit_toplevel_exn_continuation denv in
      let at_unit_toplevel =
        (* We try to show that [handler] postdominates [body] (which is done by
           showing that [body] can only return through [cont]) and that if
           [body] raises any exceptions then it only does so to toplevel.
           If this can be shown and we are currently at the toplevel of a
           compilation unit, the handler for the environment can remain marked
           as toplevel (and suitable for "let symbol" bindings); otherwise, it
           cannot. *)
        DE.at_unit_toplevel denv
          && (not (Continuation_handler.is_exn_handler cont_handler))
          && Continuation.Set.subset
               (Name_occurrences.continuations (Expr.free_names body))
               (Continuation.Set.of_list [cont; unit_toplevel_exn_cont])
      in
      let dacc, prior_lifted_constants =
        (* We clear the lifted constants accumulator so that we can easily
           obtain, below, any constants that are generated during the
           simplification of the [body].  We will add these
           [prior_lifted_constants] back into [dacc] later. *)
        DA.get_and_clear_lifted_constants dacc
      in
      let inlining_depth_increment_at_let_cont =
        DE.get_inlining_depth_increment (DA.denv dacc)
      in
      let body, handler, user_data, uacc =
        let body, (result, uenv', user_data), uacc =
          let scope = DE.get_continuation_scope_level (DA.denv dacc) in
          let params_and_handler = CH.params_and_handler cont_handler in
          let is_exn_handler = CH.is_exn_handler cont_handler in
          CPH.pattern_match params_and_handler ~f:(fun params ~handler ->
            let denv_before_body =
              DE.define_parameters (DA.denv dacc) ~params
            in
            let dacc_for_body =
              DE.increment_continuation_scope_level denv_before_body
              |> DA.with_denv dacc
            in
            (*
            Format.eprintf "BEFORE BODY (level %a):@ %a\n%!"
              Scope.print (DE.get_continuation_scope_level
                (DA.denv dacc_for_body))
              DE.print denv_before_body;
            *)
            assert (DA.no_lifted_constants dacc_for_body);
            simplify_expr dacc_for_body body (fun dacc_after_body ->
              let cont_uses_env = DA.continuation_uses_env dacc_after_body in
              let code_age_relation_after_body =
                TE.code_age_relation (DA.typing_env dacc_after_body)
              in
              let consts_lifted_during_body =
                DA.get_lifted_constants dacc_after_body
              in
              let uses =
                CUE.compute_handler_env cont_uses_env cont ~params
                  (* CR mshinwell: rename this parameter, the env does not
                     have the constants in it now *)
                  ~env_at_fork_plus_params_and_consts:denv_before_body
                  ~consts_lifted_during_body
                  ~code_age_relation_after_body
              in
              let dacc =
                (* CR mshinwell: Improve function names to clarify that this
                   function (unlike the function of the same name in [DE])
                   does not add to the environment, only to the accumulator. *)
                DA.add_lifted_constants dacc_after_body prior_lifted_constants
              in
              let handler, user_data, uacc, is_single_inlinable_use,
                  is_single_use =
                match uses with
                | No_uses ->
                  (* Don't simplify the handler if there aren't any uses:
                     otherwise, its code will be deleted but any continuation
                     usage information collected during its simplification will
                     remain. *)
                  let user_data, uacc = k dacc in
                  cont_handler, user_data, uacc, false, false
                (* CR mshinwell: Refactor so we don't have the
                   [is_single_use] hack.  The problem is that we want to
                   have the code of the handler available if we might want to
                   substitute it into a Switch---which we only want to do if
                   we won't duplicate code (i.e. if there is only one use)
                   ---but this is not a normal "inlinable" position and cannot
                   be treated as such (e.g. for join calculations). *)
                | Uses { handler_env; arg_types_by_use_id;
                         extra_params_and_args; is_single_inlinable_use;
                         is_single_use; } ->
                  (* CR mshinwell: Finish this and do it for code IDs too
                  Symbol.Set.iter (fun symbol ->
                      if not (TE.mem handler_typing_env (Name.symbol symbol))
                      then begin
                        Misc.fatal_errorf "Missing lifted constant \
                            symbol:@ %a@ handler_typing_env:@ %a@ \
                            dacc:@ %a"
                          Symbol.print symbol
                          TE.print handler_typing_env
                          DA.print dacc
                      end)
                    (LCS.all_defined_symbols
                    *)
                  let handler_typing_env, extra_params_and_args =
                    let handler_typing_env = DE.typing_env handler_env in
                    (* Unbox the parameters of the continuation if possible.
                       Any such unboxing will induce a rewrite (or wrapper) on
                       the application sites of the continuation. *)
                    match Continuation.sort cont with
                    | Normal when is_single_inlinable_use ->
                      assert (not is_exn_handler);
                      handler_typing_env, extra_params_and_args
                    | Normal | Define_root_symbol ->
                      assert (not is_exn_handler);
                      let param_types =
                        TE.find_params handler_typing_env params
                      in
                      Unbox_continuation_params.make_unboxing_decisions
                        handler_typing_env ~arg_types_by_use_id ~params
                        ~param_types extra_params_and_args
                    | Return | Toplevel_return ->
                      assert (not is_exn_handler);
                      handler_typing_env, extra_params_and_args
                    | Exn ->
                      assert is_exn_handler;
                      handler_typing_env, extra_params_and_args
                  in
                  let handler_env =
                    DE.with_typing_env handler_env handler_typing_env
                  in
                  let dacc =
                    let denv =
                      (* Install the environment arising from the join into
                         [dacc].  Note that this environment doesn't just
                         contain the joined types; it may also contain
                         definitions of code that were produced during
                         simplification of the body.  (The [DE] component of
                         [dacc_after_body] is discarded since we are now
                         moving into a different scope.) *)
                      DE.set_at_unit_toplevel_state handler_env
                        at_unit_toplevel
                    in
                    let denv =
                      (* In the case where the continuation is going to be
                         inlined, [denv] is basically the use environment,
                         which might have a deeper inlining depth increment
                         (e.g. where an [Apply] was inlined, revealing the
                         linear inlinable use of the continuation).  We need
                         to make sure the handler is simplified using the
                         depth at the [Let_cont]. *)
                      DE.set_inlining_depth_increment denv
                        inlining_depth_increment_at_let_cont
                    in
                    DA.with_denv dacc denv
                  in
                  (* CR mshinwell: This is currently disabled
                  let dacc, handler =
                    (* When still at toplevel, attempt to reify the types
                       of the continuation's parameters, to allow more lifting
                       (e.g. of closures with computed free variables). *)
                    match Continuation.sort cont with
                    | Normal
                       when is_single_inlinable_use
                         || not at_unit_toplevel -> dacc, handler
                    | Return | Toplevel_return | Exn -> dacc, handler
                    | Normal | Define_root_symbol ->
                      (* CR mshinwell: This shouldn't be [assert] in the
                         [Define_root_symbol] case *)
                      assert at_unit_toplevel;
                      Lift_inconstants.
                        lift_via_reification_of_continuation_param_types dacc
                          ~params ~extra_params_and_args ~handler
                  in *)
                  try
                    let handler, user_data, uacc =
                      simplify_one_continuation_handler dacc cont Non_recursive
                        cont_handler ~params ~handler ~extra_params_and_args k
                    in
                    handler, user_data, uacc, is_single_inlinable_use,
                      is_single_use
                  with Misc.Fatal_error -> begin
                    if !Clflags.flambda_context_on_error then begin
                      Format.eprintf "\n%sContext is:%s simplifying \
                          continuation handler (inlinable? %b)@ %a@ with \
                          [extra_params_and_args]@ %a@ \
                          with downwards accumulator:@ %a\n"
                        (Flambda_colours.error ())
                        (Flambda_colours.normal ())
                        is_single_inlinable_use
                        CH.print cont_handler
                        Continuation_extra_params_and_args.print
                        extra_params_and_args
                        DA.print dacc
                    end;
                    raise Misc.Fatal_error
                  end
              in
              let uenv = UA.uenv uacc in
              let uenv_to_return = uenv in
              let uenv =
                match uses with
                | No_uses -> uenv
                | Uses _ ->
                  let can_inline =
                    (* CR mshinwell: This check must line up with
                       Continuation_uses.  Remove the duplication. *)
                    if is_single_inlinable_use && (not is_exn_handler) then
                      Some handler
                    else
                      None
                  in
                  match can_inline with
                  | Some handler ->
                    (* CR mshinwell: tidy up *)
                    let arity =
                      match CH.behaviour handler with
                      | Unreachable { arity; }
                      | Alias_for { arity; _ }
                      | Unknown { arity; } -> arity
                    in
                    UE.add_continuation_to_inline uenv cont scope arity
                      handler
                  | None ->
                    match CH.behaviour handler with
                    | Unreachable { arity; } ->
                      UE.add_unreachable_continuation uenv cont scope arity
                    | Alias_for { arity; alias_for; } ->
                      UE.add_continuation_alias uenv cont arity ~alias_for
                    | Unknown { arity; } ->
                      if is_single_use then
                        UE.add_continuation_with_handler uenv cont scope arity
                          handler
                      else
                        UE.add_continuation uenv cont scope arity
              in
              let uacc = UA.with_uenv uacc uenv in
              (handler, uenv_to_return, user_data), uacc))
        in
        (* The upwards environment of [uacc] is replaced so that out-of-scope
           continuation bindings do not end up in the accumulator. *)
        let uacc = UA.with_uenv uacc uenv' in
        body, result, user_data, uacc
      in
      Let_cont.create_non_recursive cont handler ~body, user_data, uacc)

(* CR mshinwell: We should not simplify recursive continuations with no
   entry point -- could loop forever.  (Need to think about this again.) *)
and simplify_recursive_let_cont_handlers
  : 'a. DA.t -> Recursive_let_cont_handlers.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc rec_handlers k ->
  let module CH = Continuation_handler in
  let module CPH = Continuation_params_and_handler in
  Recursive_let_cont_handlers.pattern_match rec_handlers
    ~f:(fun ~body rec_handlers ->
      assert (not (Continuation_handlers.contains_exn_handler rec_handlers));
      let definition_denv = DA.denv dacc in
      let original_cont_scope_level =
        DE.get_continuation_scope_level definition_denv
      in
      let handlers = Continuation_handlers.to_map rec_handlers in
      let cont, cont_handler =
        match Continuation.Map.bindings handlers with
        | [] | _ :: _ :: _ ->
          Misc.fatal_errorf "Support for simplification of multiply-recursive \
              continuations is not yet implemented"
        | [c] -> c
      in
      let params_and_handler = CH.params_and_handler cont_handler in
      CPH.pattern_match params_and_handler ~f:(fun params ~handler ->
        let arity = KP.List.arity params in
        let dacc =
          DA.map_denv dacc ~f:DE.increment_continuation_scope_level
        in
        let dacc, prior_lifted_constants =
          (* We clear the lifted constants accumulator so that we can easily
             obtain, below, any constants that are generated during the
             simplification of the [body].  We will add these
             [prior_lifted_constants] back into [dacc] later. *)
          DA.get_and_clear_lifted_constants dacc
        in
        (* let set = Continuation_handlers.domain rec_handlers in *)
        let body, (handlers, user_data), uacc =
          simplify_expr dacc body (fun dacc_after_body ->
            let cont_uses_env = DA.continuation_uses_env dacc_after_body in
            let arg_types =
              (* We can't know a good type from the call types *)
              List.map T.unknown arity
            in
            (* CR mshinwell: This next part is dubious, use the rewritten
               version in the recursive-continuation-unboxing branch. *)
            let (cont_uses_env, _apply_cont_rewrite_id) :
              Continuation_uses_env.t * Apply_cont_rewrite_id.t =
              (* We don't know anything, it's like it was called
                 with an arbitrary argument! *)
              CUE.record_continuation_use cont_uses_env cont
                Non_inlinable (* Maybe simpler ? *)
                ~env_at_use:(
                  (* not useful as we will have only top *)
                  definition_denv
                )
                ~arg_types
            in
            let denv =
              (* XXX These don't have the same scope level as the
                 non-recursive case *)
              DE.add_parameters definition_denv params ~param_types:arg_types
            in
            let code_age_relation_after_body =
              TE.code_age_relation (DA.typing_env dacc_after_body)
            in
            let denv =
              DA.get_lifted_constants dacc_after_body
              |> DE.add_lifted_constants denv
            in
            let typing_env =
              TE.with_code_age_relation (DE.typing_env denv)
                code_age_relation_after_body
            in
            let denv = DE.with_typing_env denv typing_env in
            let dacc =
              DA.with_denv dacc_after_body denv
              |> DA.with_continuation_uses_env ~cont_uses_env
            in
            let dacc = DA.add_lifted_constants dacc prior_lifted_constants in
            let dacc = DA.map_denv dacc ~f:DE.set_not_at_unit_toplevel in
            let handler, user_data, uacc =
              simplify_one_continuation_handler dacc cont Recursive
                cont_handler ~params ~handler
                ~extra_params_and_args:
                  Continuation_extra_params_and_args.empty
                (fun dacc ->
                  let user_data, uacc = k dacc in
                  let uacc =
                    UA.map_uenv uacc ~f:(fun uenv ->
                      UE.add_continuation uenv cont
                        original_cont_scope_level
                        arity)
                  in
                  user_data, uacc)
            in
            let uacc =
              UA.map_uenv uacc ~f:(fun uenv ->
                UE.add_continuation_with_handler uenv cont
                  original_cont_scope_level
                  arity handler)
            in
            let handlers = Continuation.Map.singleton cont handler in
            (handlers, user_data), uacc)
        in
        let expr = Flambda.Let_cont.create_recursive handlers ~body in
        expr, user_data, uacc))

and simplify_let_cont
  : 'a. DA.t -> Let_cont.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc (let_cont : Let_cont.t) k ->
  match let_cont with
  | Non_recursive { handler; _ } ->
    simplify_non_recursive_let_cont_handler dacc handler k
  | Recursive handlers ->
    simplify_recursive_let_cont_handlers dacc handlers k

and simplify_direct_tuple_application
  : 'a. DA.t -> Apply.t -> Code_id.t
    -> 'a k -> Expr.t * 'a * UA.t
= fun dacc apply code_id k ->
  let dbg = Apply.dbg apply in
  let callee's_params_and_body = DE.find_code (DA.denv dacc) code_id in
  let param_arity =
    Function_params_and_body.params_arity callee's_params_and_body
  in
  let n = List.length param_arity in
  (* Split the tuple argument from other potential
     over application arguments *)
  let tuple, over_application_args =
    match Apply.args apply with
    | tuple :: others -> tuple, others
    | _ ->
      Misc.fatal_errorf "Empty argument list for direct application"
  in
  (* create the list of variables and projections *)
  let vars_and_fields = List.init n (fun i ->
    let var = Variable.create "tuple_field" in
    let e = Simplify_common.project_tuple ~dbg ~size:n ~field:i tuple in
    var, e)
  in
  (* Change the application to operate on the fields of the tuple *)
  let apply = Apply.with_args apply @@ (
    List.map (fun (v, _) -> Simple.var v) vars_and_fields
    @ over_application_args)
  in
  (* Immediately simplify over_applications to avoid having direct
     applications with the wrong arity. *)
  let apply_expr =
    match over_application_args with
    | [] -> Expr.create_apply apply
    | _ -> Simplify_common.split_direct_over_application apply ~param_arity
  in
  (* Insert the projections and simplify the new expression,
     to allow field projections to be simplified, and
     over-application/full_application optimizations *)
  let expr = List.fold_right (fun (v, defining_expr) body ->
    let var_bind = Var_in_binding_pos.create v Name_mode.normal in
    Expr.create_let var_bind defining_expr body
  ) vars_and_fields apply_expr
  in
  simplify_expr dacc expr k

and simplify_direct_full_application
  : 'a. DA.t -> Apply.t
    -> (T.Function_declaration_type.Inlinable.t * Rec_info.t) option
    -> result_arity:Flambda_arity.t
    -> 'a k -> Expr.t * 'a * UA.t
= fun dacc apply function_decl_opt ~result_arity k ->
  let callee = Apply.callee apply in
  let args = Apply.args apply in
(*
Format.eprintf "Considering inlining:@ %a\n%!" Apply.print apply;
*)
  let inlined =
    match function_decl_opt with
    | None -> None
    | Some (function_decl, function_decl_rec_info) ->
      let apply_inlining_depth = Apply.inlining_depth apply in
(*
Format.eprintf "apply inlining depth = %d\n%!" apply_inlining_depth;
Format.eprintf "function_decl_rec_info = %a\n%!"
  Rec_info.print function_decl_rec_info;
*)
      let decision =
        Inlining_decision.make_decision_for_call_site (DA.denv dacc)
          ~function_decl_rec_info
          ~apply_inlining_depth
          (Apply.inline apply)
      in
      match Inlining_decision.Call_site_decision.can_inline decision with
      | Do_not_inline ->
(*
Format.eprintf "Not inlining (%a) %a\n%!"
  Inlining_decision.Call_site_decision.print decision
  Apply.print apply;
*)
        None
      | Inline { unroll_to; } ->
        let dacc, inlined =
          Inlining_transforms.inline dacc ~callee
            ~args function_decl
            ~apply_return_continuation:(Apply.continuation apply)
            ~apply_exn_continuation:(Apply.exn_continuation apply)
            ~apply_inlining_depth ~unroll_to
            (Apply.dbg apply)
        in
        Some (dacc, inlined)
  in
  match inlined with
  | Some (dacc, inlined) ->
  (*
Format.eprintf "Simplifying inlined body with DE depth delta = %d\n%!"
  (DE.get_inlining_depth_increment (DA.denv dacc));
  *)
    simplify_expr dacc inlined k
  | None ->
    let dacc, use_id =
      match Apply.continuation apply with
      | Never_returns -> dacc, None
      | Return apply_return_continuation ->
        let dacc, use_id =
          DA.record_continuation_use dacc apply_return_continuation Non_inlinable
            ~env_at_use:(DA.denv dacc)
            ~arg_types:(T.unknown_types_from_arity result_arity)
        in
        dacc, Some use_id
    in
    let dacc, exn_cont_use_id =
      DA.record_continuation_use dacc
        (Exn_continuation.exn_handler (Apply.exn_continuation apply))
        Non_inlinable
        ~env_at_use:(DA.denv dacc)
        ~arg_types:(T.unknown_types_from_arity (
          Exn_continuation.arity (Apply.exn_continuation apply)))
    in
    let user_data, uacc = k dacc in
    let apply =
      Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
        apply
    in
    let expr =
      match use_id with
      | None -> Expr.create_apply apply
      | Some use_id ->
        Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
          result_arity apply
    in
    expr, user_data, uacc

and simplify_direct_partial_application
  : 'a. DA.t -> Apply.t -> callee's_code_id:Code_id.t
    -> callee's_closure_id:Closure_id.t
    -> param_arity:Flambda_arity.t -> result_arity:Flambda_arity.t
    -> recursive:Recursive.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc apply ~callee's_code_id ~callee's_closure_id
      ~param_arity ~result_arity ~recursive k ->
  (* For simplicity, we disallow [@inline] attributes on partial
     applications.  The user may always write an explicit wrapper instead
     with such an attribute. *)
  (* CR-someday mshinwell: Pierre noted that we might like a function to be
     inlined when applied to its first set of arguments, e.g. for some kind
     of type class like thing. *)
  let args = Apply.args apply in
  let dbg = Apply.dbg apply in
  let apply_continuation =
    match Apply.continuation apply with
    | Never_returns ->
      Misc.fatal_error "cannot simplify a partial application that never returns"
    | Return continuation -> continuation
  in
  begin match Apply.inline apply with
  | Always_inline | Never_inline ->
    Location.prerr_warning (Debuginfo.to_location dbg)
      (Warnings.Inlining_impossible "[@inlined] attributes may not be used \
        on partial applications")
  | Unroll _ ->
    Location.prerr_warning (Debuginfo.to_location dbg)
      (Warnings.Inlining_impossible "[@unroll] attributes may not be used \
        on partial applications")
  | Default_inline -> ()
  end;
  let arity = List.length param_arity in
  assert (arity > List.length args);
  let applied_args, remaining_param_arity =
    Misc.Stdlib.List.map2_prefix (fun arg kind ->
        if not (K.equal kind K.value) then begin
          Misc.fatal_errorf "Non-[value] kind in partial application: %a"
            Apply.print apply
        end;
        arg)
      args param_arity
  in
  begin match result_arity with
  | [kind] when K.equal kind K.value -> ()
  | _ ->
    Misc.fatal_errorf "Partially-applied function with non-[value] \
        return kind: %a"
      Apply.print apply
  end;
  let wrapper_var = Variable.create "partial_app" in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let wrapper_closure_id =
    Closure_id.wrap compilation_unit (Variable.create "partial_app_closure")
  in
  let wrapper_taking_remaining_args, dacc, dummy_code =
    let return_continuation = Continuation.create () in
    let remaining_params =
      List.map (fun kind ->
          let param = Variable.create "param" in
          Kinded_parameter.create param kind)
        remaining_param_arity
    in
    let args = applied_args @ (List.map KP.simple remaining_params) in
    let call_kind =
      Call_kind.direct_function_call callee's_code_id callee's_closure_id
        ~return_arity:result_arity
    in
    let applied_args_with_closure_vars = (* CR mshinwell: rename *)
      List.map (fun applied_arg ->
          Var_within_closure.wrap compilation_unit (Variable.create "arg"),
            applied_arg)
        ((Apply.callee apply) :: applied_args)
    in
    let my_closure = Variable.create "my_closure" in
    let exn_continuation =
      Apply.exn_continuation apply
      |> Exn_continuation.without_extra_args
    in
    let body =
      let full_application =
        Apply.create ~callee:(Apply.callee apply)
          ~continuation:(Return return_continuation)
          exn_continuation
          ~args
          ~call_kind
          dbg
          ~inline:Default_inline
          ~inlining_depth:(Apply.inlining_depth apply)
      in
      List.fold_left (fun expr (closure_var, arg) ->
          match Simple.must_be_var arg with
          | None -> expr
          | Some arg ->
            let arg = VB.create arg Name_mode.normal in
            Expr.create_let arg
              (Named.create_prim
                (Unary (Project_var {
                  project_from = wrapper_closure_id;
                  var = closure_var;
                }, Simple.var my_closure))
                dbg)
              expr)
        (Expr.create_apply full_application)
        (List.rev applied_args_with_closure_vars)
    in
    let params_and_body =
      Function_params_and_body.create ~return_continuation
        exn_continuation
        remaining_params
        ~body
        ~dbg
        ~my_closure
    in
    let code_id =
      Code_id.create
        ~name:(Closure_id.to_string callee's_closure_id ^ "_partial")
        (Compilation_unit.get_current_exn ())
    in
    let function_decl =
      Function_declaration.create ~code_id
        ~params_arity:(KP.List.arity remaining_params)
        ~result_arity
        ~stub:true
        ~dbg
        ~inline:Default_inline
        ~is_a_functor:false
        ~recursive
        ~is_tupled:false
    in
    let function_decls =
      Function_declarations.create
        (Closure_id.Lmap.singleton wrapper_closure_id function_decl)
    in
    let closure_elements =
      Var_within_closure.Map.of_list applied_args_with_closure_vars
    in
    let dummy_code =
      (* We should not add the real piece of code in the lifted constant.
         A new piece of code will always be generated when the [Let] we
         generate below is simplified.  As such we can simply add a lifted
         constant identifying deleted code.  This will ensure, if for some
         reason the constant makes it to Cmm stage, that code size is not
         increased unnecessarily. *)
      Lifted_constant.create_code code_id
        (Code (Static_const.Code.deleted code_id))
    in
    let code =
      Lifted_constant.create_code code_id
        (Code (Static_const.Code.create code_id
          ~params_and_body:(Present params_and_body)
          ~newer_version_of:None))
    in
    let dacc =
      DA.add_lifted_constant dacc dummy_code
      |> DA.map_denv ~f:(fun denv -> DE.add_lifted_constant denv code)
    in
    Set_of_closures.create function_decls ~closure_elements, dacc, dummy_code
  in
  let apply_cont =
    Apply_cont.create apply_continuation
      ~args:[Simple.var wrapper_var] ~dbg
  in
  let expr =
    let wrapper_var = VB.create wrapper_var Name_mode.normal in
    let closure_vars = [wrapper_var] in
    let pattern = Bindable_let_bound.set_of_closures ~closure_vars in
    Expr.create_pattern_let pattern
      (Named.create_set_of_closures wrapper_taking_remaining_args)
      (Expr.create_apply_cont apply_cont)
  in
  let expr, user_data, uacc = simplify_expr dacc expr k in
  let uacc = UA.add_lifted_constant_still_to_be_placed uacc dummy_code in
  expr, user_data, uacc

(* CR mshinwell: Should it be an error to encounter a non-direct application
   of a symbol after [Simplify]? This shouldn't usually happen, but I'm not 100%
   sure it cannot in every case. *)

and simplify_direct_over_application
  : 'a. DA.t -> Apply.t -> param_arity:Flambda_arity.t
    -> result_arity:Flambda_arity.t -> 'a k
    -> Expr.t * 'a * UA.t
= fun dacc apply ~param_arity ~result_arity:_ k ->
  let expr = Simplify_common.split_direct_over_application apply ~param_arity in
  simplify_expr dacc expr k

and simplify_direct_function_call
  : 'a. DA.t -> Apply.t -> callee's_code_id_from_type:Code_id.t
    -> callee's_code_id_from_call_kind:Code_id.t option
    -> callee's_closure_id:Closure_id.t
    -> result_arity:Flambda_arity.t
    -> recursive:Recursive.t -> arg_types:T.t list
    -> must_be_detupled:bool
    -> (T.Function_declaration_type.Inlinable.t * Rec_info.t) option
    -> 'a k -> Expr.t * 'a * UA.t
= fun dacc apply ~callee's_code_id_from_type ~callee's_code_id_from_call_kind
      ~callee's_closure_id ~result_arity ~recursive ~arg_types:_
      ~must_be_detupled function_decl_opt k ->
  let result_arity_of_application =
    Call_kind.return_arity (Apply.call_kind apply)
  in
  if not (Flambda_arity.equal result_arity_of_application result_arity)
  then begin
    Misc.fatal_errorf "Wrong return arity for direct OCaml function call \
        (expected %a, found %a):@ %a"
      Flambda_arity.print result_arity
      Flambda_arity.print result_arity_of_application
      Apply.print apply
  end;
  let callee's_code_id : _ Or_bottom.t =
    match callee's_code_id_from_call_kind with
    | None -> Ok callee's_code_id_from_type
    | Some callee's_code_id_from_call_kind ->
      let code_age_rel = TE.code_age_relation (DA.typing_env dacc) in
      let resolver = TE.code_age_relation_resolver (DA.typing_env dacc) in
      Code_age_relation.meet code_age_rel ~resolver
        callee's_code_id_from_call_kind callee's_code_id_from_type
  in
  match callee's_code_id with
  | Bottom ->
    let user_data, uacc = k dacc in
    Expr.create_invalid (), user_data, uacc
  | Ok callee's_code_id ->
    let call_kind =
      Call_kind.direct_function_call callee's_code_id callee's_closure_id
        ~return_arity:result_arity
    in
    let apply = Apply.with_call_kind apply call_kind in
    if must_be_detupled then
      simplify_direct_tuple_application dacc apply callee's_code_id k
    else begin
      let args = Apply.args apply in
      let provided_num_args = List.length args in
      let callee's_code = DE.find_code (DA.denv dacc) callee's_code_id in
      (* Because of tupled functions, a function declaration's params_arity
         may not match that of the underlying function_params_and_body.
         Since direct calls adopt the calling convention of the code's body
         (whereas indirect_unknown_arity calls use the convention of the
         function_declaration), we here need the arity of the callee's body *)
      let param_arity = Function_params_and_body.params_arity callee's_code in
      let num_params = List.length param_arity in
      if provided_num_args = num_params then
        simplify_direct_full_application dacc apply function_decl_opt
          ~result_arity k
      else if provided_num_args > num_params then
        simplify_direct_over_application dacc apply ~param_arity ~result_arity k
      else if provided_num_args > 0 && provided_num_args < num_params then
        simplify_direct_partial_application dacc apply ~callee's_code_id
          ~callee's_closure_id ~param_arity ~result_arity ~recursive k
      else
        Misc.fatal_errorf "Function with %d params when simplifying \
                           direct OCaml function call with %d arguments: %a"
          num_params
          provided_num_args
          Apply.print apply
    end

and simplify_function_call_where_callee's_type_unavailable
  : 'a. DA.t -> Apply.t -> Call_kind.Function_call.t
    -> args:Simple.t list -> arg_types:T.t list
    -> 'a k -> Expr.t * 'a * UA.t
= fun dacc apply (call : Call_kind.Function_call.t) ~args:_ ~arg_types k ->
  let cont =
    match Apply.continuation apply with
    | Never_returns ->
      Misc.fatal_error "cannot simplify an application that never returns"
    | Return continuation -> continuation
  in
  let denv = DA.denv dacc in
  let env_at_use = denv in
  let dacc, exn_cont_use_id =
    DA.record_continuation_use dacc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
      Non_inlinable
      ~env_at_use:(DA.denv dacc)
      ~arg_types:(T.unknown_types_from_arity (
        Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  let check_return_arity_and_record_return_cont_use ~return_arity =
(*
    let cont_arity = DA.continuation_arity dacc cont in
    if not (Flambda_arity.equal return_arity cont_arity) then begin
      Misc.fatal_errorf "Return arity (%a) on application's continuation@ \
          doesn't match return arity (%a) specified in [Call_kind]:@ %a"
        Flambda_arity.print cont_arity
        Flambda_arity.print return_arity
        Apply.print apply
    end;
*)
    DA.record_continuation_use dacc cont Non_inlinable ~env_at_use
      ~arg_types:(T.unknown_types_from_arity return_arity)
  in
  let call_kind, use_id, dacc =
    match call with
    | Indirect_unknown_arity ->
      let dacc, use_id =
        DA.record_continuation_use dacc cont Non_inlinable
          ~env_at_use ~arg_types:[T.any_value ()]
      in
      Call_kind.indirect_function_call_unknown_arity (), use_id, dacc
    | Indirect_known_arity { param_arity; return_arity; } ->
      let args_arity = T.arity_of_list arg_types in
      if not (Flambda_arity.equal param_arity args_arity) then begin
        Misc.fatal_errorf "Argument arity on indirect-known-arity \
            application doesn't match [Call_kind] (expected %a, \
            found %a):@ %a"
          Flambda_arity.print param_arity
          Flambda_arity.print args_arity
          Apply.print apply
      end;
      let dacc, use_id =
        check_return_arity_and_record_return_cont_use ~return_arity
      in
      let call_kind =
        Call_kind.indirect_function_call_known_arity ~param_arity
          ~return_arity
      in
      call_kind, use_id, dacc
    | Direct { return_arity; _ } ->
      let param_arity = T.arity_of_list arg_types in
      (* Some types have regressed in precision.  Since this used to be a
         direct call, however, we know the function's arity even though we
         don't know which function it is. *)
      let dacc, use_id =
        check_return_arity_and_record_return_cont_use ~return_arity
      in
      let call_kind =
        Call_kind.indirect_function_call_known_arity ~param_arity
          ~return_arity
      in
      call_kind, use_id, dacc
  in
  let user_data, uacc = k dacc in
  let apply =
    Apply.with_call_kind apply call_kind
    |> Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
  in
  let expr =
    Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
      (Call_kind.return_arity call_kind) apply
  in
  expr, user_data, uacc

(* CR mshinwell: I've seen at least one case where a call of kind
   [Indirect_unknown_arity] has been generated with no warning, despite having
   [@inlined always]. *)

and simplify_function_call
  : 'a. DA.t -> Apply.t -> callee_ty:T.t -> Call_kind.Function_call.t
    -> arg_types:T.t list -> 'a k
    -> Expr.t * 'a * UA.t
= fun dacc apply ~callee_ty (call : Call_kind.Function_call.t) ~arg_types k ->
  let args = Apply.args apply in
  (* Function declarations and params and body might not have the same calling
     convention. Currently the only case when it happens is for tupled functions.
     For such functions, the function_declaration declares a param_arity with a
     single argument (which is the tuple), whereas the code body takes an argument
     for each field of the tuple (the body is currified).
     When simplifying a function call, it can happen that we need to change the
     calling convention. Currently this only happens when we have a generic call
     (indirect_unknown_arity), which uses the generic/function_declaration calling
     convention, but se simplify it into a direct call, which uses the callee's code
     calling convention. In this case, we need to "detuple" the call in order to
     correctly adopt to the change in calling convention. *)
  let call_must_be_detupled is_function_decl_tupled =
    match call with
    (* In these cases, the calling convention already used in the
       application begin simplified is that of the code actually
       called. Thus we must not detuple the function *)
    | Direct _
    | Indirect_known_arity _ -> false
    (* In the indirect case, the calling convention used currently is
       the generic one. Thus we need to detuple the call iff the function
       declaration is tupled. *)
    | Indirect_unknown_arity -> is_function_decl_tupled
  in
  let type_unavailable () =
    simplify_function_call_where_callee's_type_unavailable dacc apply call
      ~args ~arg_types k
  in
  (* CR mshinwell: Should this be using [meet_shape], like for primitives? *)
  let denv = DA.denv dacc in
  match T.prove_single_closures_entry (DE.typing_env denv) callee_ty with
  | Proved (callee's_closure_id, _closures_entry, func_decl_type) ->
    (* CR mshinwell: We should check that the [set_of_closures] in the
       [closures_entry] structure in the type does indeed contain the
       closure in question. *)
    begin match func_decl_type with
    | Ok (Inlinable inlinable) ->
      let module I = T.Function_declaration_type.Inlinable in
      let callee's_code_id_from_call_kind =
        match call with
        | Direct { code_id; closure_id; _ } ->
          if not (Closure_id.equal closure_id callee's_closure_id) then begin
            Misc.fatal_errorf "Closure ID %a in application doesn't match \
                closure ID %a discovered via typing.@ Application:@ %a"
              Closure_id.print closure_id
              Closure_id.print callee's_closure_id
              Apply.print apply
          end;
          Some code_id
        | Indirect_unknown_arity
        | Indirect_known_arity _ -> None
      in
      (* CR mshinwell: This should go in Typing_env (ditto logic for Rec_info
         in Simplify_simple *)
      let function_decl_rec_info =
        let rec_info = I.rec_info inlinable in
        match Simple.rec_info (Apply.callee apply) with
        | None -> rec_info
        | Some newer -> Rec_info.merge rec_info ~newer
      in
      let callee's_code_id_from_type = I.code_id inlinable in
      let must_be_detupled = call_must_be_detupled (I.is_tupled inlinable) in
      simplify_direct_function_call dacc apply ~callee's_code_id_from_type
        ~callee's_code_id_from_call_kind ~callee's_closure_id ~arg_types
        ~result_arity:(I.result_arity inlinable)
        ~recursive:(I.recursive inlinable)
        ~must_be_detupled
        (Some (inlinable, function_decl_rec_info)) k
    | Ok (Non_inlinable non_inlinable) ->
      let module N = T.Function_declaration_type.Non_inlinable in
      let callee's_code_id_from_type = N.code_id non_inlinable in
      let callee's_code_id_from_call_kind =
        match call with
        | Direct { code_id; _ } -> Some code_id
        | Indirect_unknown_arity
        | Indirect_known_arity _ -> None
      in
      let must_be_detupled = call_must_be_detupled (N.is_tupled non_inlinable) in
      simplify_direct_function_call dacc apply ~callee's_code_id_from_type
        ~callee's_code_id_from_call_kind
        ~callee's_closure_id ~arg_types
        ~result_arity:(N.result_arity non_inlinable)
        ~recursive:(N.recursive non_inlinable)
        ~must_be_detupled
        None k
    | Bottom ->
      let user_data, uacc = k dacc in
      Expr.create_invalid (), user_data, uacc
    | Unknown -> type_unavailable ()
    end
  | Unknown -> type_unavailable ()
  | Invalid ->
    let user_data, uacc = k dacc in
    Expr.create_invalid (), user_data, uacc

and simplify_apply_shared dacc apply : _ Or_bottom.t =
(*
  DA.check_continuation_is_bound dacc (Apply.continuation apply);
  DA.check_exn_continuation_is_bound dacc (Apply.exn_continuation apply);
*)
  let min_name_mode = Name_mode.normal in
  match S.simplify_simple dacc (Apply.callee apply) ~min_name_mode with
  | Bottom, _ty -> Bottom
  | Ok callee, callee_ty ->
    match S.simplify_simples dacc (Apply.args apply) ~min_name_mode with
    | _, Bottom -> Bottom
    | _changed, Ok args_with_types ->
      let args, arg_types = List.split args_with_types in
      let inlining_depth =
        DE.get_inlining_depth_increment (DA.denv dacc)
          + Apply.inlining_depth apply
      in
(*
Format.eprintf "Apply of %a: apply's inlining depth %d, DE's delta %d\n%!"
  Simple.print callee
  (Apply.inlining_depth apply)
  (DE.get_inlining_depth_increment (DA.denv dacc));
*)
      let apply =
        Apply.create ~callee
          ~continuation:(Apply.continuation apply)
          (Apply.exn_continuation apply)
          ~args
          ~call_kind:(Apply.call_kind apply)
          (DE.add_inlined_debuginfo' (DA.denv dacc) (Apply.dbg apply))
          ~inline:(Apply.inline apply)
          ~inlining_depth
      in
      Ok (callee_ty, apply, arg_types)

and simplify_method_call
  : 'a. DA.t -> Apply.t -> callee_ty:T.t -> kind:Call_kind.method_kind
    -> obj:Simple.t -> arg_types:T.t list -> 'a k
    -> Expr.t * 'a * UA.t
= fun dacc apply ~callee_ty ~kind:_ ~obj ~arg_types k ->
  let callee_kind = T.kind callee_ty in
  if not (K.is_value callee_kind) then begin
    Misc.fatal_errorf "Method call with callee of wrong kind %a: %a"
      K.print callee_kind
      T.print callee_ty
  end;
  let apply_cont =
    match Apply.continuation apply with
    | Never_returns ->
      Misc.fatal_error "cannot simplify a method call that never returns"
    | Return continuation -> continuation
  in
  let denv = DA.denv dacc in
  DE.check_simple_is_bound denv obj;
  let expected_arity = List.map (fun _ -> K.value) arg_types in
  let args_arity = T.arity_of_list arg_types in
  if not (Flambda_arity.equal expected_arity args_arity) then begin
    Misc.fatal_errorf "All arguments to a method call must be of kind \
        [value]:@ %a"
      Apply.print apply
  end;
  let dacc, use_id =
    DA.record_continuation_use dacc apply_cont Non_inlinable
      ~env_at_use:denv
      ~arg_types:[T.any_value ()]
  in
  let dacc, exn_cont_use_id =
    DA.record_continuation_use dacc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
      Non_inlinable
      ~env_at_use:(DA.denv dacc)
      ~arg_types:(T.unknown_types_from_arity (
        Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  (* CR mshinwell: Need to record exception continuation use (check all other
     cases like this too) *)
  let user_data, uacc = k dacc in
  let apply =
    Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
      apply
  in
  let expr =
    Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
      (Flambda_arity.create [K.value]) apply
  in
  expr, user_data, uacc

and simplify_c_call
  : 'a. DA.t -> Apply.t -> callee_ty:T.t -> param_arity:Flambda_arity.t
    -> return_arity:Flambda_arity.t -> arg_types:T.t list -> 'a k
    -> Expr.t * 'a * UA.t
= fun dacc apply ~callee_ty ~param_arity ~return_arity ~arg_types k ->
  let callee_kind = T.kind callee_ty in
  if not (K.is_value callee_kind) then begin
    Misc.fatal_errorf "C callees must be of kind [value], not %a: %a"
      K.print callee_kind
      T.print callee_ty
  end;
  let args_arity = T.arity_of_list arg_types in
  if not (Flambda_arity.equal args_arity param_arity) then begin
    Misc.fatal_errorf "Arity %a of [Apply] arguments doesn't match \
        parameter arity %a of C callee:@ %a"
      Flambda_arity.print args_arity
      Flambda_arity.print param_arity
      Apply.print apply
  end;
(* CR mshinwell: We can't do these checks (here and elsewhere) on [DA]
   any more.  Maybe we can check on [UA] after calling [k] instead.
  let cont = Apply.continuation apply in
  let cont_arity = DA.continuation_arity dacc cont in
  if not (Flambda_arity.equal cont_arity return_arity) then begin
    Misc.fatal_errorf "Arity %a of [Apply] continuation doesn't match \
        return arity %a of C callee:@ %a"
      Flambda_arity.print cont_arity
      Flambda_arity.print return_arity
      Apply.print apply
  end;
*)
  let dacc, use_id =
    match Apply.continuation apply with
    | Return apply_continuation ->
      let dacc, use_id =
        DA.record_continuation_use dacc apply_continuation Non_inlinable
          ~env_at_use:(DA.denv dacc)
          ~arg_types:(T.unknown_types_from_arity return_arity)
      in
      dacc, Some use_id
    | Never_returns ->
      dacc, None
  in
  let dacc, exn_cont_use_id =
    (* CR mshinwell: Try to factor out these stanzas, here and above. *)
    DA.record_continuation_use dacc
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
      Non_inlinable
      ~env_at_use:(DA.denv dacc)
      ~arg_types:(T.unknown_types_from_arity (
        Exn_continuation.arity (Apply.exn_continuation apply)))
  in
  let user_data, uacc = k dacc in
  (* CR mshinwell: Make sure that [resolve_continuation_aliases] has been
     called before building of any term that contains a continuation *)
  let apply =
    Simplify_common.update_exn_continuation_extra_args uacc ~exn_cont_use_id
      apply
  in
  let expr =
    match use_id with
    | Some use_id ->
      Simplify_common.add_wrapper_for_fixed_arity_apply uacc ~use_id
        return_arity apply
    | None ->
      Expr.create_apply apply
  in
  expr, user_data, uacc

and simplify_apply
  : 'a. DA.t -> Apply.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc apply k ->
  match simplify_apply_shared dacc apply with
  | Bottom ->
    let user_data, uacc = k dacc in
    Expr.create_invalid (), user_data, uacc
  | Ok (callee_ty, apply, arg_types) ->
    match Apply.call_kind apply with
    | Function call ->
      simplify_function_call dacc apply ~callee_ty call ~arg_types k
    | Method { kind; obj; } ->
      simplify_method_call dacc apply ~callee_ty ~kind ~obj ~arg_types k
    | C_call { alloc = _; param_arity; return_arity; } ->
      simplify_c_call dacc apply ~callee_ty ~param_arity ~return_arity
        ~arg_types k


and simplify_apply_cont
  : 'a. DA.t -> Apply_cont.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc apply_cont k ->
  let module AC = Apply_cont in
  let min_name_mode = Name_mode.normal in
  match S.simplify_simples dacc (AC.args apply_cont) ~min_name_mode with
  | _, Bottom ->
    let user_data, uacc = k dacc in
    Expr.create_invalid (), user_data, uacc
  | _changed, Ok args_with_types ->
    let args, arg_types = List.split args_with_types in
(* CR mshinwell: Resurrect arity checks
    let args_arity = T.arity_of_list arg_types in
*)
    let use_kind : Continuation_use_kind.t =
      (* CR mshinwell: Is [Continuation.sort] reliable enough to detect
         the toplevel continuation?  Probably not -- we should store it in
         the environment. *)
      match Continuation.sort (AC.continuation apply_cont) with
      | Normal ->
        if Option.is_none (Apply_cont.trap_action apply_cont) then Inlinable
        else Non_inlinable
      | Return | Toplevel_return | Exn -> Non_inlinable
      | Define_root_symbol ->
        assert (Option.is_none (Apply_cont.trap_action apply_cont));
        Inlinable
    in
    let dacc, rewrite_id =
      DA.record_continuation_use dacc
        (AC.continuation apply_cont)
        use_kind
        ~env_at_use:(DA.denv dacc)
        ~arg_types
    in
    let user_data, uacc = k dacc in
    let uenv = UA.uenv uacc in
    let rewrite =
      UE.find_apply_cont_rewrite uenv (AC.continuation apply_cont)
    in
    let cont =
      UE.resolve_continuation_aliases uenv (AC.continuation apply_cont)
    in
    let rewrite_use_result =
      let apply_cont = AC.update_continuation_and_args apply_cont cont ~args in
      let apply_cont =
        match AC.trap_action apply_cont with
        | None -> apply_cont
        | Some (Push { exn_handler; } | Pop { exn_handler; _ }) ->
          if UE.mem_continuation uenv exn_handler then apply_cont
          else AC.clear_trap_action apply_cont
      in
      (* CR mshinwell: Could remove the option type most likely if
         [Simplify_static] was fixed to handle the toplevel exn continuation
         properly. *)
      match rewrite with
      | None -> Apply_cont_rewrite.no_rewrite apply_cont
      | Some rewrite ->
        Apply_cont_rewrite.rewrite_use rewrite rewrite_id apply_cont
     in
  (*
  Format.eprintf "Apply_cont is now %a\n%!" Expr.print apply_cont_expr;
    if !Clflags.flambda_invariant_checks then begin
      Variable.Set.iter (fun var ->
          let name = Name.var var in
          if not (TE.mem (DA.typing_env dacc) name) then begin
            Misc.fatal_errorf "[Apply_cont]@ %a after rewrite@ %a \
                contains unbound names in:@ %a"
              Expr.print apply_cont_expr
              (Misc.Stdlib.Option.print Apply_cont_rewrite.print) rewrite
              DA.print dacc
          end)
        (Name_occurrences.variables (Expr.free_names apply_cont_expr))
    end;
  *)
    let check_arity_against_args ~arity:_ = () in
  (*
      if not (Flambda_arity.equal args_arity arity) then begin
        Misc.fatal_errorf "Arity of arguments in [Apply_cont] (%a) does not \
            match continuation's arity from the environment (%a):@ %a"
          Flambda_arity.print args_arity
          Flambda_arity.print arity
          AC.print apply_cont
      end
    in
  *)
    let normal_case () =
      match rewrite_use_result with
      | Apply_cont apply_cont ->
        Expr.create_apply_cont apply_cont, user_data, uacc
      | Expr expr -> expr, user_data, uacc
    in
    match UE.find_continuation uenv cont with
    | Unknown { arity; handler = _; } ->
      check_arity_against_args ~arity;
      normal_case ()
    | Unreachable { arity; } ->
      check_arity_against_args ~arity;
      (* N.B. We allow this transformation even if there is a trap action,
         on the basis that there wouldn't be any opportunity to collect any
         backtrace, even if the [Apply_cont] were compiled as "raise". *)
      Expr.create_invalid (), user_data, uacc
    | Inline { arity; handler; } ->
      match rewrite_use_result with
      | Expr _ ->
        (* CR-someday mshinwell: Consider supporting inlining in the case of
           a non-trivial wrapper. *)
        normal_case ()
      | Apply_cont apply_cont ->
        (* CR mshinwell: With -g, we can end up with continuations that are
           just a sequence of phantom lets then "goto".  These would normally
           be treated as aliases, but of course aren't in this scenario,
           unless the continuations are used linearly. *)
        (* CR mshinwell: maybe instead of [Inline] it should say "linearly used"
           or "stub" -- could avoid resimplification of linearly used ones
           maybe, although this wouldn't remove any parameter-to-argument
           [Let]s. However perhaps [Flambda_to_cmm] could deal with these. *)
        check_arity_against_args ~arity;
        match AC.trap_action apply_cont with
        | Some _ ->
          (* Until such time as we can manually add to the backtrace buffer,
             never substitute a "raise" for the body of an exception handler. *)
          normal_case ()
        | None ->
          Flambda.Continuation_params_and_handler.pattern_match
            (Flambda.Continuation_handler.params_and_handler handler)
            ~f:(fun params ~handler ->
    (*
              let params_arity = KP.List.arity params in
              if not (Flambda_arity.equal params_arity args_arity) then begin
                Misc.fatal_errorf "Arity of arguments in [Apply_cont] does not \
                    match arity of parameters on handler (%a):@ %a"
                  Flambda_arity.print params_arity
                  AC.print apply_cont
              end;
    *)
              (* CR mshinwell: Why does [New_let_binding] have a [Variable]? *)
              (* CR mshinwell: Should verify that names in the
                 [Apply_cont_rewrite] are in scope. *)
              (* We can't easily call [simplify_expr] on the inlined body since
                 [dacc] isn't the correct accumulator and environment any more.
                 However there's no need to simplify the inlined body except to
                 make use of parameter-to-argument bindings; we just leave them
                 for a subsequent round of [Simplify] or [Un_cps] to clean
                 up. *)
              let args = Apply_cont.args apply_cont in
              let params_and_args =
                assert (List.compare_lengths params args = 0);
                List.map (fun (param, arg) ->
                    param, Named.create_simple arg)
                  (List.combine params args)
              in
              let expr =
                Expr.bind_parameters ~bindings:params_and_args ~body:handler
              in
              expr, user_data, uacc)

(* CR mshinwell: Consider again having [Switch] arms taking arguments. *)
and simplify_switch
  : 'a. DA.t -> Switch.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc switch k ->
  let module AC = Apply_cont in
  let min_name_mode = Name_mode.normal in
  let scrutinee = Switch.scrutinee switch in
  match S.simplify_simple dacc scrutinee ~min_name_mode with
  | Bottom, _ty ->
    let user_data, uacc = k dacc in
    Expr.create_invalid (), user_data, uacc
  | Ok scrutinee, scrutinee_ty ->
    let arms, dacc =
      let typing_env_at_use = DA.typing_env dacc in
      Target_imm.Map.fold (fun arm action (arms, dacc) ->
          let shape =
            let imm = Target_imm.int (Target_imm.to_targetint arm) in
            T.this_naked_immediate imm
          in
          match T.meet typing_env_at_use scrutinee_ty shape with
          | Bottom -> arms, dacc
          | Ok (_meet_ty, env_extension) ->
(*
            Format.eprintf "Scrutinee %a, action:@ %a,@ EE:@ %a\n%!"
              Simple.print scrutinee
              Apply_cont.print action
              TEE.print env_extension;
*)
            let env_at_use =
              TE.add_env_extension typing_env_at_use env_extension
              |> DE.with_typing_env (DA.denv dacc)
            in
            let args = AC.args action in
            match args with
            | [] ->
              let dacc, rewrite_id =
                DA.record_continuation_use dacc (AC.continuation action)
                  Non_inlinable ~env_at_use ~arg_types:[]
              in
              let arms = Target_imm.Map.add arm (action, rewrite_id, []) arms in
              arms, dacc
            | _::_ ->
              let min_name_mode = Name_mode.normal in
              match S.simplify_simples dacc args ~min_name_mode with
              | _, Bottom -> arms, dacc
              | _changed, Ok args_with_types ->
                let args, arg_types = List.split args_with_types in
                let dacc, rewrite_id =
                  DA.record_continuation_use dacc (AC.continuation action)
                    Non_inlinable ~env_at_use ~arg_types
                in
                let arity = List.map T.kind arg_types in
                let action = Apply_cont.update_args action ~args in
                let arms =
                  Target_imm.Map.add arm (action, rewrite_id, arity) arms
                in
                arms, dacc)
        (Switch.arms switch)
        (Target_imm.Map.empty, dacc)
    in
    let user_data, uacc = k dacc in
    let new_let_conts, arms, identity_arms, not_arms =
      Target_imm.Map.fold
        (fun arm (action, use_id, arity)
             (new_let_conts, arms, identity_arms, not_arms) ->
          match
            Simplify_common.add_wrapper_for_switch_arm uacc action
              ~use_id arity
          with
          | Apply_cont action ->
            let action =
              (* First try to absorb any [Apply_cont] expression that forms the
                 entirety of the arm's action (via an intermediate zero-arity
                 continuation without trap action) into the [Switch] expression
                 itself. *)
              if not (Apply_cont.is_goto action) then Some action
              else
                let cont = Apply_cont.continuation action in
                match UE.find_continuation (UA.uenv uacc) cont with
                | Inline { arity = _; handler; }
                | Unknown { arity = _; handler = Some handler; } ->
                  Continuation_params_and_handler.pattern_match
                    (Continuation_handler.params_and_handler handler)
                    ~f:(fun params ~handler ->
                      assert (List.length params = 0);
                      match Expr.descr handler with
                      | Apply_cont action -> Some action
                      | Let _ | Let_cont _ | Apply _
                      | Switch _ | Invalid _ -> Some action)
                | Unknown _ -> Some action
                | Unreachable _ -> None
            in
            begin match action with
            | None ->
              (* The destination is unreachable; delete the [Switch] arm. *)
              new_let_conts, arms, identity_arms, not_arms
            | Some action ->
              let normal_case ~identity_arms ~not_arms =
                let arms = Target_imm.Map.add arm action arms in
                new_let_conts, arms, identity_arms, not_arms
              in
              (* Now check to see if the arm is of a form that might mean the
                 whole [Switch] is either the identity or a boolean NOT. *)
              match Apply_cont.to_one_arg_without_trap_action action with
              | None -> normal_case ~identity_arms ~not_arms
              | Some arg ->
                (* CR-someday mshinwell: Maybe this check should be generalised
                   e.g. to detect
                     | 0 -> apply_cont k x y 1
                     | 1 -> apply_cont k x y 0
                *)
                let [@inline always] const arg =
                  match Reg_width_const.descr arg with
                  | Tagged_immediate arg ->
                    if Target_imm.equal arm arg then
                      let identity_arms =
                        Target_imm.Map.add arm action identity_arms
                      in
                      normal_case ~identity_arms ~not_arms
                    else if
                      (Target_imm.equal arm Target_imm.bool_true
                        && Target_imm.equal arg Target_imm.bool_false)
                      ||
                        (Target_imm.equal arm Target_imm.bool_false
                          && Target_imm.equal arg Target_imm.bool_true)
                    then
                      let not_arms = Target_imm.Map.add arm action not_arms in
                      normal_case ~identity_arms ~not_arms
                    else
                      normal_case ~identity_arms ~not_arms
                  | Naked_immediate _ | Naked_float _ | Naked_int32 _
                  | Naked_int64 _ | Naked_nativeint _ ->
                    normal_case ~identity_arms ~not_arms
                in
                Simple.pattern_match arg ~const
                  ~name:(fun _ -> normal_case ~identity_arms ~not_arms)
            end
          | New_wrapper (new_cont, new_handler) ->
            let new_let_cont = new_cont, new_handler in
            let new_let_conts = new_let_cont :: new_let_conts in
            let action = Apply_cont.goto new_cont in
            let arms = Target_imm.Map.add arm action arms in
            new_let_conts, arms, identity_arms, not_arms)
        arms
        ([], Target_imm.Map.empty, Target_imm.Map.empty, Target_imm.Map.empty)
    in
    let switch_is_identity =
      let arm_discrs = Target_imm.Map.keys arms in
      let identity_arms_discrs = Target_imm.Map.keys identity_arms in
      if not (Target_imm.Set.equal arm_discrs identity_arms_discrs) then
        None
      else
        Target_imm.Map.data identity_arms
        |> List.map Apply_cont.continuation
        |> Continuation.Set.of_list
        |> Continuation.Set.get_singleton
    in
    let switch_is_boolean_not =
      let arm_discrs = Target_imm.Map.keys arms in
      let not_arms_discrs = Target_imm.Map.keys not_arms in
      if (not (Target_imm.Set.equal arm_discrs Target_imm.all_bools))
        || (not (Target_imm.Set.equal arm_discrs not_arms_discrs))
      then
        None
      else
        Target_imm.Map.data not_arms
        |> List.map Apply_cont.continuation
        |> Continuation.Set.of_list
        |> Continuation.Set.get_singleton
    in
    let create_tagged_scrutinee k =
      let bound_to = Variable.create "tagged_scrutinee" in
      let bound_vars =
        Bindable_let_bound.singleton (VB.create bound_to NM.normal)
      in
      let named =
        Named.create_prim (Unary (Box_number Untagged_immediate, scrutinee))
          Debuginfo.none
      in
      let dacc =
        (* Disable inconstant lifting *)
        DA.map_denv dacc ~f:DE.set_not_at_unit_toplevel
      in
      match Simplify_named.simplify_named dacc bound_vars named with
      | Bindings
          { bindings_outermost_first = bindings; dacc = _; } ->
        let body = k ~tagged_scrutinee:(Simple.var bound_to) in
        Simplify_common.bind_let_bound ~bindings ~body, user_data, uacc
      | Reified _ ->
        Misc.fatal_errorf "Simplify_switch: Inconstant lifting is disabled, \
          so Simplify_named.simplify_named should not return Reify"
      | Shared symbol ->
        let body = k ~tagged_scrutinee:(Simple.symbol symbol) in
        body, user_data, uacc
    in
    let body, user_data, uacc =
      match switch_is_identity with
      | Some dest ->
        create_tagged_scrutinee (fun ~tagged_scrutinee ->
          let apply_cont =
            Apply_cont.create dest ~args:[tagged_scrutinee] ~dbg:Debuginfo.none
          in
          Expr.create_apply_cont apply_cont)
      | None ->
        match switch_is_boolean_not with
        | Some dest ->
          create_tagged_scrutinee (fun ~tagged_scrutinee ->
            let not_scrutinee = Variable.create "not_scrutinee" in
            let apply_cont =
              Apply_cont.create dest ~args:[Simple.var not_scrutinee]
                ~dbg:Debuginfo.none
            in
            Expr.create_let (VB.create not_scrutinee NM.normal)
              (Named.create_prim (P.Unary (Boolean_not, tagged_scrutinee))
                Debuginfo.none)
              (Expr.create_apply_cont apply_cont))
        | None ->
          let expr = Expr.create_switch ~scrutinee ~arms in
          if !Clflags.flambda_invariant_checks
            && Simple.is_const scrutinee
            && Target_imm.Map.cardinal arms > 1
          then begin
            Misc.fatal_errorf "[Switch] with constant scrutinee (type: %a) \
                should have been simplified away:@ %a"
              T.print scrutinee_ty
              Expr.print expr
          end;
          expr, user_data, uacc
    in
    let expr =
      List.fold_left (fun body (new_cont, new_handler) ->
          Let_cont.create_non_recursive new_cont new_handler ~body)
        body
        new_let_conts
    in
    expr, user_data, uacc

and simplify_expr
  : 'a. DA.t -> Expr.t -> 'a k -> Expr.t * 'a * UA.t
= fun dacc expr k ->
  match Expr.descr expr with
  | Let let_expr -> simplify_let dacc let_expr k
  | Let_cont let_cont -> simplify_let_cont dacc let_cont k
  | Apply apply -> simplify_apply dacc apply k
  | Apply_cont apply_cont -> simplify_apply_cont dacc apply_cont k
  | Switch switch -> simplify_switch dacc switch k
  | Invalid _ ->
    (* CR mshinwell: Make sure that a program can be simplified to just
       [Invalid].  [Un_cps] should translate any [Invalid] that it sees as if
       it were [Halt_and_catch_fire]. *)
    let user_data, uacc = k dacc in
    expr, user_data, uacc
