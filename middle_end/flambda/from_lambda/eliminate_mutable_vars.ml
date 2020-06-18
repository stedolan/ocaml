(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2019 OCamlPro SAS                                    *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Env : sig
  type t

  val create
     : return_continuation:Continuation.t
    -> exn_continuation:Ilambda.exn_continuation
    -> t

  val add_mutable_and_make_new_id
     : t
    -> Ident.t
    -> Lambda.value_kind
    -> t * Ident.t

  val new_id_for_mutable : t -> Ident.t -> t * Ident.t * Lambda.value_kind

  type add_continuation_result = private {
    body_env : t;
    handler_env : t;
    extra_params : (Ident.t * Lambda.value_kind) list;
  }

  val add_continuation
     : t
    -> Continuation.t
    -> Asttypes.rec_flag
    -> add_continuation_result

  val extra_args_for_continuation : t -> Continuation.t -> Ident.t list

  val extra_args_for_continuation_with_kinds
     : t
    -> Continuation.t
    -> (Ident.t * Lambda.value_kind) list

  val rename_variable : t -> Ident.t -> Ident.t

  val rename_simple : t -> Ilambda.simple -> Ilambda.simple
  val rename_simples : t -> Ilambda.simple list -> Ilambda.simple list
end = struct
  type t = {
    current_values_of_mutables_in_scope
      : (Ident.t * Lambda.value_kind) Ident.Map.t;
    mutables_needed_by_continuations : Ident.Set.t Continuation.Map.t;
  }

  let create ~return_continuation
        ~(exn_continuation : Ilambda.exn_continuation) =
    let mutables_needed_by_continuations =
      Continuation.Map.of_list [
        return_continuation, Ident.Set.empty;
        exn_continuation.exn_handler, Ident.Set.empty;
      ]
    in
    { current_values_of_mutables_in_scope = Ident.Map.empty;
      mutables_needed_by_continuations;
    }

  let add_mutable_and_make_new_id t id kind =
    if Ident.Map.mem id t.current_values_of_mutables_in_scope then begin
      Misc.fatal_errorf "Redefinition of mutable variable %a"
        Ident.print id
    end;
    let new_id = Ident.rename id in
    let current_values_of_mutables_in_scope =
      Ident.Map.add id (new_id, kind) t.current_values_of_mutables_in_scope
    in
    let t =
      { t with
        current_values_of_mutables_in_scope;
      }
    in
    t, new_id

  let new_id_for_mutable t id =
    match Ident.Map.find id t.current_values_of_mutables_in_scope with
    | exception Not_found ->
      Misc.fatal_errorf "Mutable variable %a not in environment"
        Ident.print id
    | _old_id, kind ->
      let new_id = Ident.rename id in
      let current_values_of_mutables_in_scope =
        Ident.Map.add id (new_id, kind) t.current_values_of_mutables_in_scope
      in
      let t =
        { t with
          current_values_of_mutables_in_scope;
        }
      in
      t, new_id, kind

  let mutables_in_scope t =
    Ident.Map.keys t.current_values_of_mutables_in_scope

  type add_continuation_result = {
    body_env : t;
    handler_env : t;
    extra_params : (Ident.t * Lambda.value_kind) list;
  }

  let add_continuation t cont (recursive : Asttypes.rec_flag) =
    let body_env =
      let mutables_needed_by_continuations =
        Continuation.Map.add cont (mutables_in_scope t)
          t.mutables_needed_by_continuations
      in
      { t with
        mutables_needed_by_continuations;
      }
    in
    let current_values_of_mutables_in_scope =
      Ident.Map.mapi (fun mut_var (_outer_value, kind) ->
          Ident.rename mut_var, kind)
        t.current_values_of_mutables_in_scope
    in
    let handler_env =
      let handler_env =
        match recursive with
        | Nonrecursive -> t
        | Recursive -> body_env
      in
      { handler_env with
        current_values_of_mutables_in_scope;
      }
    in
    let extra_params =
      List.map snd
        (Ident.Map.bindings handler_env.current_values_of_mutables_in_scope)
    in
    { body_env;
      handler_env;
      extra_params;
    }

  let extra_args_for_continuation_with_kinds t cont =
    match Continuation.Map.find cont t.mutables_needed_by_continuations with
    | exception Not_found ->
      Misc.fatal_errorf "Unbound continuation %a" Continuation.print cont
    | mutables ->
      let mutables = Ident.Set.elements mutables in
      List.map (fun mut ->
          match Ident.Map.find mut t.current_values_of_mutables_in_scope with
          | exception Not_found ->
            Misc.fatal_errorf "No current value for %a" Ident.print mut
          | current_value, kind -> current_value, kind)
        mutables

  let extra_args_for_continuation t cont =
    List.map fst (extra_args_for_continuation_with_kinds t cont)

  let rename_variable t id =
    match Ident.Map.find id t.current_values_of_mutables_in_scope with
    | exception Not_found -> id
    | (id, _kind) -> id

  let rename_simple t (simple : Ilambda.simple) : Ilambda.simple =
    match simple with
    | Var id -> Var (rename_variable t id)
    | Const _ -> simple

  let rename_simples t simples =
    List.map (fun simple -> rename_simple t simple) simples
end

let transform_exn_continuation env
      (exn_continuation : Ilambda.exn_continuation)
      : Ilambda.exn_continuation =
  let more_extra_args =
    Env.extra_args_for_continuation_with_kinds env exn_continuation.exn_handler
  in
  let more_extra_args =
    List.map (fun (arg, kind) : (Ilambda.simple * _) -> Var arg, kind)
      more_extra_args
  in
  { exn_handler = exn_continuation.exn_handler;
    extra_args = exn_continuation.extra_args @ more_extra_args;
  }

let rec transform_expr env (expr : Ilambda.t) : Ilambda.t =
  match expr with
  | Let (id, user_visible, kind, named, body) ->
    transform_named env id user_visible kind named (fun env : Ilambda.t ->
      let body = transform_expr env body in
      body)
  | Let_mutable let_mutable -> transform_let_mutable env let_mutable
  | Let_rec (func_decls, body) ->
    let func_decls =
      List.map (fun (id, func_decl) ->
          id, transform_function_declaration env func_decl)
        func_decls
    in
    let body = transform_expr env body in
    Let_rec (func_decls, body)
  | Let_cont let_cont -> Let_cont (transform_let_cont env let_cont)
  | Apply ({ exn_continuation; _ } as apply) ->
    let apply = transform_apply env apply in
    let exn_continuation = transform_exn_continuation env exn_continuation in
    let extra_args = Env.extra_args_for_continuation env apply.continuation in
    begin match extra_args with
    | [] -> Apply { apply with exn_continuation; }
    | _::_ ->
      let wrapper_cont = Continuation.create () in
      let return_value = Ident.create_local "return_val" in
      let args =
        List.map (fun var : Ilambda.simple -> Var var)
          (return_value :: extra_args)
      in
      Let_cont {
        name = wrapper_cont;
        is_exn_handler = false;
        params = [return_value, Not_user_visible, Pgenval];
        recursive = Nonrecursive;
        body = Apply {
          apply with
          continuation = wrapper_cont;
          exn_continuation;
        };
        handler = Apply_cont (apply.continuation, None, args);
      }
    end
  | Apply_cont (cont, trap_action, args) ->
    let args = Env.rename_simples env args in
    let extra_args =
      List.map (fun var : Ilambda.simple -> Var var)
        (Env.extra_args_for_continuation env cont)
    in
    Apply_cont (cont, trap_action, args @ extra_args)
  | Switch (id, switch) ->
    let id = Env.rename_variable env id in
    let consts_rev =
      List.fold_left (fun consts_rev (arm, cont, trap, args) ->
          let args = Env.rename_simples env args in
          let extra_args = Env.extra_args_for_continuation env cont in
          let extra_args =
            List.map (fun arg : Ilambda.simple -> Var arg)
              extra_args
          in
          (arm, cont, trap, args @ extra_args) :: consts_rev)
        []
        switch.consts
    in
    let consts = List.rev consts_rev in
    let failaction =
      match switch.failaction with
      | None -> None
      | Some (cont, trap, args) ->
        let args = Env.rename_simples env args in
        let extra_args = Env.extra_args_for_continuation env cont in
        let extra_args =
          List.map (fun arg : Ilambda.simple -> Var arg)
            extra_args
        in
        Some (cont, trap, args @ extra_args)
    in
    let switch =
      { switch with
        consts;
        failaction;
      }
    in
    Ilambda.Switch (id, switch)

and transform_named env id user_visible kind (named : Ilambda.named) k
      : Ilambda.t =
  let normal_case named : Ilambda.t =
    Let (id, user_visible, kind, named, k env)
  in
  match named with
  | Simple (Var id) -> normal_case (Simple (Var (Env.rename_variable env id)))
  | Simple (Const _) -> normal_case named
  | Prim { prim; args; loc; exn_continuation; } ->
    let args = Env.rename_simples env args in
    let exn_continuation =
      Option.map (transform_exn_continuation env) exn_continuation
    in
    normal_case (Prim { prim; args; loc; exn_continuation; })
  | Assign { being_assigned; new_value; } ->
    let env, new_id, new_kind = Env.new_id_for_mutable env being_assigned in
    Let (new_id, User_visible, new_kind,
      Simple (Env.rename_simple env new_value),
      Let (id, Not_user_visible, kind,
        Simple (Const (Const_base (Const_int 0))), k env))

and transform_let_mutable env
      ({ id; initial_value; contents_kind; body; } : Ilambda.let_mutable)
      : Ilambda.t =
  let env, new_id = Env.add_mutable_and_make_new_id env id contents_kind in
  let body = transform_expr env body in
  Let (new_id, User_visible, contents_kind, Simple initial_value, body)

and transform_let_cont env
      ({ name; is_exn_handler; params; recursive; body; handler; }
         : Ilambda.let_cont)
      : Ilambda.let_cont =
  let { Env. body_env; handler_env; extra_params } =
    Env.add_continuation env name recursive
  in
  let extra_params =
    List.map (fun (id, kind) -> id, Ilambda.User_visible, kind) extra_params
  in
  { name;
    is_exn_handler;
    params = params @ extra_params;
    recursive;
    body = transform_expr body_env body;
    handler = transform_expr handler_env handler;
  }

and transform_apply env
      ({ kind; func; args; continuation; exn_continuation;
         loc; should_be_tailcall; inlined; specialised;
       } : Ilambda.apply) : Ilambda.apply =
  let kind : Ilambda.apply_kind =
    match kind with
    | Function -> Function
    | Method { kind; obj; } ->
      Method {
        kind;
        obj = Env.rename_simple env obj;
      }
  in
  { kind;
    func = Env.rename_variable env func;
    args = Env.rename_simples env args;
    continuation;
    exn_continuation;
    loc;
    should_be_tailcall;
    inlined;
    specialised;
  }

and transform_function_declaration _env
      ({ kind; return_continuation; exn_continuation; params; return;
         body; free_idents_of_body; attr; loc; stub;
       } : Ilambda.function_declaration) : Ilambda.function_declaration =
  { kind;
    return_continuation;
    exn_continuation;
    params;
    return;
    body = transform_toplevel ~return_continuation ~exn_continuation body;
    free_idents_of_body;
    attr;
    loc;
    stub;
  }

and transform_toplevel ~return_continuation ~exn_continuation expr =
  transform_expr (Env.create ~return_continuation ~exn_continuation) expr

let run (program : Ilambda.program) =
  let expr =
    transform_toplevel ~return_continuation:program.return_continuation
      ~exn_continuation:program.exn_continuation
      program.expr
  in
  { program with
    expr;
  }
