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

open! Simplify_import

let simplify_projection dacc ~original_term ~deconstructing ~shape ~result_var
      ~result_kind =
  let env = DA.typing_env dacc in
  match T.meet_shape env deconstructing ~shape ~result_var ~result_kind with
  | Bottom -> Reachable.invalid (), TEE.empty (), dacc
  | Ok env_extension ->
    Reachable.reachable original_term, env_extension, dacc

type cse =
  | Invalid of T.t
  | Applied of (Reachable.t * TEE.t * DA.t)
  | Not_applied of DA.t

let apply_cse dacc ~original_prim =
  match P.Eligible_for_cse.create original_prim with
  | None -> None
  | Some with_fixed_value ->
    let typing_env = DA.typing_env dacc in
    match TE.find_cse typing_env with_fixed_value with
    | None -> None
    | Some simple ->
      match TE.get_canonical_simple_exn typing_env simple with
      | exception Not_found -> None
      | simple -> Some simple

let try_cse dacc ~original_prim ~result_kind ~min_name_mode
      ~result_var : cse =
  (* CR mshinwell: Use [meet] and [reify] for CSE?  (discuss with lwhite) *)
  if not (Name_mode.equal min_name_mode Name_mode.normal) then Not_applied dacc
  else
    match apply_cse dacc ~original_prim with
    | Some replace_with ->
      let named = Named.create_simple replace_with in
      let ty = T.alias_type_of result_kind replace_with in
      let env_extension = TEE.one_equation (Name.var result_var) ty in
      Applied (Reachable.reachable named, env_extension, dacc)
    | None ->
      let dacc =
        match P.Eligible_for_cse.create original_prim with
        | None -> dacc
        | Some eligible_prim ->
          let bound_to = Simple.var result_var in
          DA.map_denv dacc ~f:(fun denv ->
            DE.with_typing_env denv
            (TE.add_cse (DE.typing_env denv) eligible_prim ~bound_to))
      in
      Not_applied dacc

type add_wrapper_for_fixed_arity_continuation0_result =
  | This_continuation of Continuation.t
  | Apply_cont of Flambda.Apply_cont.t
  | New_wrapper of Continuation.t * Flambda.Continuation_handler.t

type cont_or_apply_cont =
  | Continuation of Continuation.t
  | Apply_cont of Apply_cont.t

let add_wrapper_for_fixed_arity_continuation0 uacc cont_or_apply_cont
      ~use_id arity : add_wrapper_for_fixed_arity_continuation0_result =
  let uenv = UA.uenv uacc in
  let cont =
    match cont_or_apply_cont with
    | Continuation cont -> cont
    | Apply_cont apply_cont -> Apply_cont.continuation apply_cont
  in
  let original_cont = cont in
  let cont = UE.resolve_continuation_aliases uenv cont in
  match UE.find_apply_cont_rewrite uenv original_cont with
  | None -> This_continuation cont
  | Some rewrite when Apply_cont_rewrite.does_nothing rewrite ->
    let arity_in_rewrite = Apply_cont_rewrite.original_params_arity rewrite in
    if not (Flambda_arity.equal arity arity_in_rewrite) then begin
      Misc.fatal_errorf "Arity %a provided to fixed-arity-wrapper \
          addition function does not match arity %a in rewrite:@ %a"
        Flambda_arity.print arity
        Flambda_arity.print arity_in_rewrite
        Apply_cont_rewrite.print rewrite
    end;
    This_continuation cont
  | Some rewrite ->
    (* CR-someday mshinwell: This area should be improved and hence
       simplified.  Allowing [Apply] to take extra arguments is probably the
       way forward.  Although unboxing of variants requires untagging
       expressions to be inserted, so wrappers cannot always be avoided. *)
    let params = List.map (fun _kind -> Variable.create "param") arity in
    let kinded_params = List.map2 KP.create params arity in
    let new_wrapper expr =
      let new_cont = Continuation.create () in
      let new_handler =
        let params_and_handler =
          Continuation_params_and_handler.create kinded_params ~handler:expr
        in
        Continuation_handler.create ~params_and_handler
          ~stub:false
          ~is_exn_handler:false
      in
      New_wrapper (new_cont, new_handler)
    in
    match cont_or_apply_cont with
    | Continuation cont ->
      (* In this case, any generated [Apply_cont] will sit inside a wrapper
         that binds [kinded_params]. *)
      let args = List.map KP.simple kinded_params in
      let apply_cont = Apply_cont.create cont ~args ~dbg:Debuginfo.none in
      begin match Apply_cont_rewrite.rewrite_use rewrite use_id apply_cont with
      | Apply_cont apply_cont -> new_wrapper (Expr.create_apply_cont apply_cont)
      | Expr expr -> new_wrapper expr
      end
    | Apply_cont apply_cont ->
      let apply_cont = Apply_cont.update_continuation apply_cont cont in
      match Apply_cont_rewrite.rewrite_use rewrite use_id apply_cont with
      | Apply_cont apply_cont -> Apply_cont apply_cont
      | Expr expr -> new_wrapper expr

type add_wrapper_for_switch_arm_result =
  | Apply_cont of Flambda.Apply_cont.t
  | New_wrapper of Continuation.t * Flambda.Continuation_handler.t

let add_wrapper_for_switch_arm uacc apply_cont ~use_id arity
      : add_wrapper_for_switch_arm_result =
  match
    add_wrapper_for_fixed_arity_continuation0 uacc (Apply_cont apply_cont)
      ~use_id arity
  with
  | This_continuation cont ->
    Apply_cont (Apply_cont.update_continuation apply_cont cont)
  | Apply_cont apply_cont -> Apply_cont apply_cont
  | New_wrapper (cont, wrapper) -> New_wrapper (cont, wrapper)

let add_wrapper_for_fixed_arity_continuation uacc cont ~use_id arity ~around =
  match
    add_wrapper_for_fixed_arity_continuation0 uacc (Continuation cont)
      ~use_id arity
  with
  | This_continuation cont -> around cont
  | Apply_cont _ -> assert false
  | New_wrapper (new_cont, new_handler) ->
    Let_cont.create_non_recursive new_cont new_handler ~body:(around new_cont)

let add_wrapper_for_fixed_arity_apply uacc ~use_id arity apply =
  match Apply.continuation apply with
  | Never_returns ->
    Expr.create_apply apply
  | Return cont ->
    add_wrapper_for_fixed_arity_continuation uacc cont
      ~use_id arity
      ~around:(fun return_cont ->
        let exn_cont =
          UE.resolve_exn_continuation_aliases (UA.uenv uacc)
            (Apply.exn_continuation apply)
        in
        let apply =
          Apply.with_continuations apply (Return return_cont) exn_cont
        in
        Expr.create_apply apply)

let update_exn_continuation_extra_args uacc ~exn_cont_use_id apply =
  let exn_cont_rewrite =
    UE.find_apply_cont_rewrite (UA.uenv uacc)
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
  in
  match exn_cont_rewrite with
  | None -> apply
  | Some rewrite ->
    Apply.with_exn_continuation apply
      (Apply_cont_rewrite.rewrite_exn_continuation rewrite exn_cont_use_id
        (Apply.exn_continuation apply))

(* CR mshinwell: Should probably move [Reachable] into the [Flambda] recursive
   loop and then move this into [Expr].  Maybe this could be tidied up a bit
   too? *)
let bind_let_bound ~bindings ~body =
  List.fold_left
    (fun expr
         ((bound : Bindable_let_bound.t), (defining_expr : Reachable.t)) ->
      match defining_expr with
      | Invalid _ -> Expr.create_invalid ()
      | Reachable defining_expr ->
        match bound with
        | Singleton var -> Expr.bind ~bindings:[var, defining_expr] ~body:expr
        | Set_of_closures _ -> Expr.create_pattern_let bound defining_expr expr)
    body
    (List.rev bindings)

let create_let_symbol0 r code_age_relation (bound_symbols : Bound_symbols.t)
      (static_const : Static_const.t) body =
(*
  Format.eprintf "create_let_symbol %a\n%!" Bound_symbols.print bound_symbols;
*)
  let free_names_after = Expr.free_names body in
  match bound_symbols with
  | Singleton sym ->
    if not (Name_occurrences.mem_symbol free_names_after sym) then body, r
    else
      let expr =
        Let_symbol.create Syntactic bound_symbols static_const body
        |> Expr.create_let_symbol
      in
      expr, r
  | Sets_of_closures _ ->
    let bound_names_unused =
      let being_defined =
        Bound_symbols.everything_being_defined bound_symbols
      in
      Code_id_or_symbol.Set.for_all
        (fun (code_id_or_symbol : Code_id_or_symbol.t) ->
          match code_id_or_symbol with
          | Code_id code_id ->
            not (Name_occurrences.mem_code_id free_names_after code_id)
          | Symbol sym ->
            not (Name_occurrences.mem_symbol free_names_after sym))
        being_defined
    in
    let all_code_ids_bound_names =
      let bound_names = Bound_symbols.free_names bound_symbols in
      Name_occurrences.code_ids_and_newer_version_of_code_ids bound_names
    in
    let newer_version_of_code_ids_after =
      Name_occurrences.newer_version_of_code_ids free_names_after
    in
    let code_ids_after =
      Name_occurrences.code_ids free_names_after
    in
    let code_ids_only_used_in_newer_version_of_after =
      Code_id.Set.diff newer_version_of_code_ids_after code_ids_after
    in
    let all_code_ids_free_names_after =
      Code_id.Set.union newer_version_of_code_ids_after code_ids_after
    in
    (* CR mshinwell: Add [Set.are_disjoint]? *)
    if bound_names_unused
      && Code_id.Set.is_empty (Code_id.Set.inter
        all_code_ids_bound_names all_code_ids_free_names_after)
    then body, r
    else
      (* Turn pieces of code that are only referenced in [newer_version_of]
         fields into [Deleted]. *)
      let code_ids_to_make_deleted =
        (* CR-someday mshinwell: This could be made more precise, but would
           probably require a proper analysis. *)
        let code_ids_static_const =
          Name_occurrences.code_ids (Static_const.free_names static_const)
        in
        let code_ids_only_used_in_newer_version_of =
          Code_id.Set.inter all_code_ids_bound_names
            (Code_id.Set.diff code_ids_only_used_in_newer_version_of_after
              code_ids_static_const)
        in
        (* We cannot delete code unless it is certain that a non-trivial join
           operation between later versions of it cannot happen. *)
        Code_id.Set.filter (fun code_id ->
            (* CR mshinwell: Think again about whether we need to have these
               two separate calls. *)
            Code_age_relation.newer_versions_form_linear_chain
              code_age_relation code_id
              ~all_code_ids_still_existing:all_code_ids_bound_names
            &&
            Code_age_relation.newer_versions_form_linear_chain
              code_age_relation code_id
              ~all_code_ids_still_existing:all_code_ids_free_names_after)
          code_ids_only_used_in_newer_version_of
      in
      let sets =
        List.map (fun code_and_set_of_closures ->
            Static_const.Code_and_set_of_closures.map_code
              code_and_set_of_closures
              ~f:(fun code_id code ->
                if Code_id.Set.mem code_id code_ids_to_make_deleted
                then Static_const.Code.make_deleted code
                else code))
          (Static_const.must_be_sets_of_closures static_const)
      in
      let static_const = Static_const.Sets_of_closures sets in
      let expr =
        Let_symbol.create Syntactic bound_symbols static_const body
        |> Expr.create_let_symbol
      in
      let r =
        R.remember_code_for_cmx r
          (Static_const.get_pieces_of_code' static_const
           |> Code_id.Lmap.bindings
           |> Code_id.Map.of_list)
      in
      expr, r

let remove_unused_closure_vars0 r
      ({ code; set_of_closures; } : Static_const.Code_and_set_of_closures.t)
      : Static_const.Code_and_set_of_closures.t =
  let closure_elements =
    Set_of_closures.closure_elements set_of_closures
    |> Var_within_closure.Map.filter (fun closure_var _ ->
      Var_within_closure.Set.mem closure_var (R.used_closure_vars r))
  in
  let set_of_closures =
    Set_of_closures.create (Set_of_closures.function_decls set_of_closures)
      ~closure_elements
  in
  { code;
    set_of_closures;
  }

let remove_unused_closure_vars r (static_const : Static_const.t)
      : Static_const.t =
  match static_const with
  | Sets_of_closures code_and_sets ->
    let code_and_sets =
      List.map (remove_unused_closure_vars0 r) code_and_sets
    in
    Sets_of_closures code_and_sets
  | Block _
  | Boxed_float _
  | Boxed_int32 _
  | Boxed_int64 _
  | Boxed_nativeint _
  | Immutable_float_block _
  | Immutable_float_array _
  | Mutable_string _
  | Immutable_string _ -> static_const

let create_let_symbol r (scoping_rule : Let_symbol.Scoping_rule.t)
      code_age_relation bound_symbols static_const body =
  let static_const = remove_unused_closure_vars r static_const in
  match scoping_rule with
  | Syntactic ->
    create_let_symbol0 r code_age_relation bound_symbols static_const body
  | Dominator ->
    let expr =
      Let_symbol.create Dominator bound_symbols static_const body
      |> Expr.create_let_symbol
    in
    let r =
      R.remember_code_for_cmx r
        (Static_const.get_pieces_of_code' static_const
         |> Code_id.Lmap.bindings
         |> Code_id.Map.of_list)
    in
    expr, r
