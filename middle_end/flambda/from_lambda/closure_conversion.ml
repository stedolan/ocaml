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

[@@@ocaml.warning "+a-4-30-40-41-42-66"]

open! Int_replace_polymorphic_compare
open! Flambda

module Env = Closure_conversion_aux.Env
module Function_decls = Closure_conversion_aux.Function_decls
module Function_decl = Function_decls.Function_decl
module Let_symbol = Flambda.Let_symbol_expr

module K = Flambda_kind
module LC = Lambda_conversions
module P = Flambda_primitive
module VB = Var_in_binding_pos

type t = {
  backend : (module Flambda_backend_intf.S);
  current_unit_id : Ident.t;
  symbol_for_global' : (Ident.t -> Symbol.t);
  filename : string;
  ilambda_exn_continuation : Continuation.t;
  mutable imported_symbols : Symbol.Set.t;
  (* All symbols in [imported_symbols] are to be of kind [Value]. *)
  mutable declared_symbols : (Symbol.t * Static_const.t) list;
  mutable shareable_constants : Symbol.t Static_const.Map.t;
  mutable code : (Code_id.t * Function_params_and_body.t) list;
}

(* To avoid excessive nesting of continuations, we lift [Let_cont] expressions
   higher, dropping them when one of their free names is about to go out of
   scope. *)

module Handler_SCC = Strongly_connected_components.Make (Continuation)

let drop_handlers0 handlers ~around:body =
  (* When dropping some number of handlers after a binding, we need to get
     them in the correct order, as there may be dependencies between them.
     The dependencies can only be on continuations. *)
  let top_sorted =
    let graph =
      Continuation.Map.map (fun (_handler, free_names_of_handler) ->
          free_names_of_handler
          |> Name_occurrences.continuations)
        handlers
    in
    Handler_SCC.connected_components_sorted_from_roots_to_leaf graph
  in
  Array.fold_left (fun body (component : Handler_SCC.component) ->
      match component with
      | No_loop cont ->
        let handler, _ = Continuation.Map.find cont handlers in
        Flambda.Let_cont.create_non_recursive cont handler ~body
      | Has_loop conts ->
        let handlers =
          conts
          |> List.map (fun cont ->
               cont, fst (Continuation.Map.find cont handlers))
          |> Continuation.Map.of_list
        in
        Flambda.Let_cont.create_recursive handlers ~body)
    body
    top_sorted

let drop_handlers handlers ~leaving_scope_of ~around:body =
  let to_drop =
    (* If a free variable or continuation is about to go out of scope and
       a continuation [k] depends on such variable or continuation, then the
       continuation [k] must be dropped just after the corresponding binding,
       together with any other continuations which have [k] free in them. *)
    Delayed_handlers.find_rev_deps handlers leaving_scope_of
  in
  let body = drop_handlers0 to_drop ~around:body in
  let handlers = Delayed_handlers.remove_domain_of_map handlers to_drop in
  body, handlers

let drop_all_handlers handlers ~around =
  drop_handlers0 (Delayed_handlers.all handlers) ~around

let symbol_for_ident t id =
  let symbol = t.symbol_for_global' id in
  t.imported_symbols <- Symbol.Set.add symbol t.imported_symbols;
  Simple.symbol symbol

(* Generate a wrapper ("stub") function that accepts a tuple argument and calls
   another function with arguments extracted in the obvious manner from the
   tuple. *)
let tupled_function_call_stub
      (original_params : (Variable.t * Lambda.value_kind) list)
      (unboxed_version : Closure_id.t)
      code_id ~(closure_id : Closure_id.t)
      recursive =
  let dbg = Debuginfo.none in
  let return_continuation = Continuation.create ~sort:Return () in
  let exn_continuation =
    let exn_handler = Continuation.create ~sort:Exn () in
    Exn_continuation.create ~exn_handler ~extra_args:[]
  in
  let tuple_param_var =
    Variable.rename ~append:"tupled_stub_param"
      (Closure_id.unwrap unboxed_version)
  in
  let my_closure = Variable.create "my_closure" in
  let params = List.map (fun (var, _) -> Variable.rename var) original_params in
  let unboxed_version_var = Variable.create "unboxed_version" in
  let call =
    let call_kind =
      Call_kind.direct_function_call code_id unboxed_version
        ~return_arity:[K.value]
    in
    let apply =
      Flambda.Apply.create ~callee:(Simple.var unboxed_version_var)
        ~continuation:(Return return_continuation)
        exn_continuation
        ~args:(Simple.vars params)
        ~call_kind
        Debuginfo.none
        ~inline:Default_inline
        ~inlining_depth:0
    in
    Expr.create_apply apply
  in
  let body_with_closure_bound =
    let move : Flambda_primitive.unary_primitive =
      Select_closure {
        move_from = closure_id;
        move_to = unboxed_version;
      }
    in
    let unboxed_version_var =
      Var_in_binding_pos.create unboxed_version_var Name_mode.normal
    in
    Expr.create_let unboxed_version_var
      (Named.create_prim (Unary (move, Simple.var my_closure)) dbg)
      call
  in
  let _, body =
    List.fold_left (fun (pos, body) param ->
        let defining_expr =
          let pos = Target_imm.int (Targetint.OCaml.of_int pos) in
          let block_access : P.Block_access_kind.t =
            Values {
              tag = Tag.Scannable.zero;
              size = Known (Targetint.OCaml.of_int (List.length params));
              field_kind = Any_value;
            }
          in
          Named.create_prim
            (Binary (
              Block_load (block_access, Immutable),
              Simple.var tuple_param_var,
              Simple.const (Reg_width_const.tagged_immediate pos)))
            dbg
        in
        let param = VB.create param Name_mode.normal in
        let expr = Expr.create_let param defining_expr body in
        pos + 1, expr)
      (0, body_with_closure_bound)
      params
  in
  let tuple_param = Kinded_parameter.create tuple_param_var K.value in
  let params_and_body =
    Flambda.Function_params_and_body.create
      ~return_continuation
      exn_continuation
      [tuple_param]
      ~dbg
      ~body
      ~my_closure
  in
  let code_id =
    Code_id.create ~name:((Closure_id.to_string closure_id) ^ "_tuple_stub")
      (Compilation_unit.get_current_exn ())
  in
  let func_decl =
    Flambda.Function_declaration.create ~code_id
      ~params_arity:[K.value]
      ~result_arity:[K.value]
      ~stub:true
      ~dbg
      ~inline:Default_inline
      ~is_a_functor:false
      ~recursive
  in
  func_decl, code_id, params_and_body

let register_const0 t constant name =
  match Static_const.Map.find constant t.shareable_constants with
  | exception Not_found ->
    (* Create a variable to ensure uniqueness of the symbol. *)
    let var = Variable.create name in
    let symbol =
      Symbol.create (Compilation_unit.get_current_exn ())
        (Linkage_name.create
           (Variable.unique_name (Variable.rename var)))
    in
    t.declared_symbols <- (symbol, constant) :: t.declared_symbols;
    if Static_const.can_share constant then begin
      t.shareable_constants
        <- Static_const.Map.add constant symbol t.shareable_constants
    end;
    symbol
  | symbol -> symbol

let register_const t constant name : Static_const.Field_of_block.t * string =
  let symbol = register_const0 t constant name in
  Symbol symbol, name

let register_const_string t str =
  register_const0 t (Static_const.Immutable_string str) "string"

let rec declare_const t (const : Lambda.structured_constant)
      : Static_const.Field_of_block.t * string =
  match const with
  | Const_base (Const_int c) ->
    Tagged_immediate (Target_imm.int (Targetint.OCaml.of_int c)), "int"
  | Const_pointer p ->
    (* CR mshinwell: This needs to be removed. *)
    Tagged_immediate (Target_imm.int (Targetint.OCaml.of_int p)), "const_ptr"
  | Const_base (Const_char c) -> Tagged_immediate (Target_imm.char c), "char"
  | Const_base (Const_string (s, _)) ->
    let const, name =
      if Config.safe_string then
        Static_const.Immutable_string s, "immstring"
      else
        Static_const.Mutable_string { initial_value = s; }, "string"
    in
    register_const t const name
  | Const_base (Const_float c) ->
    let c = Numbers.Float_by_bit_pattern.create (float_of_string c) in
    register_const t (Boxed_float (Const c)) "float"
  | Const_base (Const_int32 c) ->
    register_const t (Boxed_int32 (Const c)) "int32"
  | Const_base (Const_int64 c) ->
    register_const t (Boxed_int64 (Const c)) "int64"
  | Const_base (Const_nativeint c) ->
    (* CR pchambart: this should be pushed further to lambda *)
    let c = Targetint.of_int64 (Int64.of_nativeint c) in
    register_const t (Boxed_nativeint (Const c)) "nativeint"
  | Const_immstring c ->
    register_const t (Immutable_string c) "immstring"
  | Const_float_block c ->
    register_const t
      (Immutable_float_block
         (List.map (fun s ->
           let f = Numbers.Float_by_bit_pattern.create (float_of_string s) in
           Or_variable.Const f) c))
      "float_block"
  | Const_float_array c ->
    register_const t
      (Immutable_float_array
         (List.map (fun s ->
           let f = Numbers.Float_by_bit_pattern.create (float_of_string s) in
           Or_variable.Const f) c))
      "float_array"
  | Const_block (tag, consts) ->
    let const : Static_const.t  =
      Block
        (Tag.Scannable.create_exn tag, Immutable,
         List.map (fun c -> fst (declare_const t c)) consts)
    in
    register_const t const "const_block"

let close_const0 t (const : Lambda.structured_constant) =
  match declare_const t const with
  | Tagged_immediate c, name ->
    Simple.const (Reg_width_const.tagged_immediate c), name
  | Symbol s, name -> Simple.symbol s, name
  | Dynamically_computed _, name ->
    Misc.fatal_errorf "Declaring a computed constant %s" name

let close_const t const =
  let simple, name = close_const0 t const in
  Named.create_simple simple, name

let find_simple_from_id _t env id =
  match Env.find_var_exn env id with
  | exception Not_found ->
    Misc.fatal_errorf
      "find_simple_from_id: Cannot find [Ident] %a in environment"
      Ident.print id
  | var ->
    match Env.find_simple_to_substitute_exn env id with
    | exception Not_found -> Simple.var var
    | simple -> simple

(* CR mshinwell: Avoid the double lookup *)
let find_simple t env (simple : Ilambda.simple) =
  match simple with
  | Const const ->
    let simple, _ = close_const0 t const in
    simple
  | Var id -> find_simple_from_id t env id

let find_simples t env ids =
  List.map (fun id -> find_simple t env id) ids

let close_c_call t ~let_bound_var (prim : Primitive.description)
      ~(args : Simple.t list) exn_continuation dbg
      (k : Named.t option -> Expr.t * _) : Expr.t * _ =
  (* XCR pchambart: there should be a special case if body is a
     apply_cont
     mshinwell: done. *)
  (* We always replace the original Ilambda [Let] with an Flambda
     expression, so we call [k] with [None], to get just the closure-converted
     body of that [Let]. *)
  let body, delayed_handlers = k None in
  let return_continuation, needs_wrapper =
    match Flambda.Expr.descr body with
    | Apply_cont apply_cont
      when
        Simple.List.equal (Apply_cont_expr.args apply_cont)
          [Simple.var let_bound_var]
        && Option.is_none (Apply_cont_expr.trap_action apply_cont) ->
      Apply_cont_expr.continuation apply_cont, false
    | _ -> Continuation.create (), true
  in
  let param_arity =
    List.map LC.kind_of_primitive_native_repr prim.prim_native_repr_args
  in
  let return_kind =
    LC.kind_of_primitive_native_repr prim.prim_native_repr_res
  in
  let return_arity = [return_kind] in
  let call_kind =
    Call_kind.c_call ~alloc:prim.prim_alloc ~param_arity ~return_arity
  in
  let call_symbol =
    let prim_name =
      if String.equal prim.prim_native_name "" then prim.prim_name
      else prim.prim_native_name
    in
    (* CR mshinwell: fix "extern" mess (see Un_cps) *)
    Symbol.create (Compilation_unit.external_symbols ())
      (Linkage_name.create prim_name)
  in
  t.imported_symbols <- Symbol.Set.add call_symbol t.imported_symbols;
  let call args =
    let apply =
      Flambda.Apply.create ~callee:(Simple.symbol call_symbol)
        ~continuation:(Return return_continuation)
        exn_continuation
        ~args
        ~call_kind
        dbg
        ~inline:Default_inline
        ~inlining_depth:0
    in
    Flambda.Expr.create_apply apply
  in
  let call : Flambda.Expr.t =
    List.fold_left2 (fun (call : Simple.t list -> Expr.t)
            arg (arg_repr : Primitive.native_repr) ->
        let unbox_arg : P.unary_primitive option =
          match arg_repr with
          | Same_as_ocaml_repr -> None
          | Unboxed_float -> Some (P.Unbox_number Naked_float)
          | Unboxed_integer Pnativeint -> Some (P.Unbox_number Naked_nativeint)
          | Unboxed_integer Pint32 -> Some (P.Unbox_number Naked_int32)
          | Unboxed_integer Pint64 -> Some (P.Unbox_number Naked_int64)
          | Untagged_int -> Some (P.Unbox_number Untagged_immediate)
        in
        match unbox_arg with
        | None -> (fun args -> call (arg :: args))
        | Some named ->
          (fun args ->
             let unboxed_arg = Variable.create "unboxed" in
             let unboxed_arg' =
               VB.create unboxed_arg Name_mode.normal
             in
             Expr.create_let unboxed_arg'
               (Named.create_prim (Unary (named, arg)) dbg)
               (call ((Simple.var unboxed_arg) :: args))))
      call
      args
      prim.prim_native_repr_args
      []
  in
  let code_after_call, handler_param, needs_wrapper =
    let box_return_value =
      match prim.prim_native_repr_res with
      | Same_as_ocaml_repr -> None
      | Unboxed_float -> Some (P.Box_number Naked_float)
      | Unboxed_integer Pnativeint -> Some (P.Box_number Naked_nativeint)
      | Unboxed_integer Pint32 -> Some (P.Box_number Naked_int32)
      | Unboxed_integer Pint64 -> Some (P.Box_number Naked_int64)
      | Untagged_int -> Some (P.Box_number Untagged_immediate)
    in
    match box_return_value with
    | None -> body, let_bound_var, needs_wrapper
    | Some box_return_value ->
      let let_bound_var' = VB.create let_bound_var Name_mode.normal in
      let handler_param = Variable.rename let_bound_var in
      let body =
        Flambda.Expr.create_let let_bound_var'
          (Named.create_prim
            (Unary (box_return_value, Simple.var handler_param))
            dbg)
          body
      in
      body, handler_param, true
  in
  let call =
    if not needs_wrapper then call
    else
      let after_call =
        let params = [Kinded_parameter.create handler_param return_kind] in
        let params_and_handler =
          Flambda.Continuation_params_and_handler.create params
            ~handler:code_after_call
        in
        Flambda.Continuation_handler.create ~params_and_handler
          ~stub:false
          ~is_exn_handler:false
      in
      Flambda.Let_cont.create_non_recursive return_continuation after_call
        ~body:call
  in
  call, delayed_handlers

let close_exn_continuation t env (exn_continuation : Ilambda.exn_continuation) =
  let extra_args =
    List.map (fun (simple, kind) ->
        let simple = find_simple t env simple in
        simple, LC.value_kind kind)
      exn_continuation.extra_args
  in
  Exn_continuation.create ~exn_handler:exn_continuation.exn_handler ~extra_args

let close_primitive t env ~let_bound_var named (prim : Lambda.primitive) ~args
      loc (exn_continuation : Ilambda.exn_continuation option)
      (k : Named.t option -> Expr.t * _) : Expr.t * _ =
  let exn_continuation =
    match exn_continuation with
    | None -> None
    | Some exn_continuation ->
      Some (close_exn_continuation t env exn_continuation)
  in
  let args = find_simples t env args in
  let dbg = Debuginfo.from_location loc in
  match prim, args with
  | Pccall prim, args ->
    let exn_continuation =
      match exn_continuation with
      | None ->
        Misc.fatal_errorf "Pccall is missing exception continuation: %a"
          Ilambda.print_named named
      | Some exn_continuation -> exn_continuation
    in
    close_c_call t ~let_bound_var prim ~args exn_continuation dbg k
  | Pgetglobal id, [] ->
    let is_predef_exn = Ident.is_predef id in
    if not (is_predef_exn || not (Ident.same id t.current_unit_id))
    then begin
      Misc.fatal_errorf "Non-predef Pgetglobal %a in the same unit"
        Ident.print id
    end;
    k (Some (Named.create_simple (symbol_for_ident t id)))
  | Praise raise_kind, [_] ->
    let exn_continuation =
      match exn_continuation with
      | None ->
        Misc.fatal_errorf "Praise is missing exception continuation: %a"
          Ilambda.print_named named
      | Some exn_continuation -> exn_continuation
    in
    let exn_handler = Exn_continuation.exn_handler exn_continuation in
    let args =
      (* CR mshinwell: Share with [Lambda_to_flambda_primitives_helpers] *)
      let extra_args =
        List.map (fun (simple, _kind) -> simple)
          (Exn_continuation.extra_args exn_continuation)
      in
      args @ extra_args
    in
    let raise_kind = Some (LC.raise_kind raise_kind) in
    let trap_action = Trap_action.Pop { exn_handler; raise_kind; } in
    let apply_cont =
      Flambda.Apply_cont.create ~trap_action exn_handler ~args ~dbg
    in
    (* Since raising of an exception doesn't terminate, we don't call [k]. *)
    Flambda.Expr.create_apply_cont apply_cont, Delayed_handlers.empty
  | prim, args ->
    Lambda_to_flambda_primitives.convert_and_bind exn_continuation
      ~backend:t.backend
      ~register_const_string:(register_const_string t)
      prim ~args dbg k

let close_trap_action_opt trap_action =
  Option.map (fun (trap_action : Ilambda.trap_action) : Trap_action.t ->
      match trap_action with
      | Push { exn_handler; } -> Push { exn_handler; }
      | Pop { exn_handler; } ->
        Pop { exn_handler; raise_kind = None; })
    trap_action

let rec close t env (ilam : Ilambda.t) : Expr.t * _ =
  match ilam with
  | Let (id, user_visible, _kind, defining_expr, body) ->
    (* CR mshinwell: Remove [kind] on the Ilambda terms? *)
    let body_env, var = Env.add_var_like env id user_visible in
    let cont (defining_expr : Named.t option) =
      let body_env =
        match defining_expr with
        | Some (Simple simple) ->
          Env.add_simple_to_substitute body_env id simple
        | Some _ | None -> body_env
      in
      (* CR pchambart: Not tail ! *)
      let body, handlers = close t body_env body in
      let body, handlers =
        let leaving_scope_of =
          Name_occurrences.singleton_variable var Name_mode.normal
        in
        drop_handlers handlers ~leaving_scope_of ~around:body
      in
      match defining_expr with
      | None -> body, handlers
      | Some defining_expr ->
        let var = VB.create var Name_mode.normal in
        Expr.create_let var defining_expr body, handlers
    in
    close_named t env ~let_bound_var:var defining_expr cont
  | Let_mutable _ ->
    Misc.fatal_error "[Let_mutable] should have been removed by \
      [Eliminate_mutable_vars]"
  | Let_rec (defs, body) -> close_let_rec t env ~defs ~body
  | Let_cont { name; is_exn_handler; params; recursive; body;
      handler; } ->
    if is_exn_handler then begin
      match recursive with
      | Nonrecursive -> ()
      | Recursive ->
        Misc.fatal_errorf "[Let_cont]s marked as exception handlers must \
            be [Nonrecursive]: %a"
          Ilambda.print ilam
    end;
    let params_with_kinds = params in
    let handler_env, params =
      Env.add_vars_like env
        (List.map (fun (param, user_visible, _kind) -> param, user_visible)
          params)
    in
    let params =
      List.map2 (fun param (_, _, kind) ->
          Kinded_parameter.create param (LC.value_kind kind))
        params
        params_with_kinds
    in
    let body, delayed_handlers_body = close t env body in
    (* If we are still at toplevel, don't un-nest handlers, otherwise we
       may produce situations where reification of continuation parameters'
       types (yielding new [Let_symbol] bindings) cause code IDs or symbols to
       go out of syntactic scope (but not out of dominator scope). *)
    let still_at_toplevel =
      (* Same calculation as in [Simplify_expr]. *)
      Env.still_at_toplevel env
        && not (Flambda_features.Expert.denest_at_toplevel ())
        && (not is_exn_handler)
        && Continuation.Set.subset
              (Name_occurrences.continuations (Expr.free_names body))
              (Continuation.Set.of_list [name; t.ilambda_exn_continuation])
    in
    let body, delayed_handlers_body =
      if still_at_toplevel then
        drop_all_handlers delayed_handlers_body ~around:body,
          Delayed_handlers.empty
      else
        let leaving_scope_of =
          Name_occurrences.singleton_continuation name
        in
        drop_handlers delayed_handlers_body ~leaving_scope_of ~around:body
    in
    let handler_env =
      if still_at_toplevel then handler_env
      else Env.no_longer_at_toplevel handler_env
    in
    let handler, delayed_handlers_handler = close t handler_env handler in
    let handler, delayed_handlers_handler =
      if still_at_toplevel then
        drop_all_handlers delayed_handlers_handler ~around:handler,
          Delayed_handlers.empty
      else
        let leaving_scope_of =
          Name_occurrences.add_continuation_in_trap_action
            (Name_occurrences.add_continuation
              (Kinded_parameter.List.free_names params)
              name)
            name
        in
        drop_handlers delayed_handlers_handler ~leaving_scope_of
          ~around:handler
    in
    let params_and_handler =
      Flambda.Continuation_params_and_handler.create params ~handler
    in
    let handler =
      Flambda.Continuation_handler.create ~params_and_handler
        ~stub:false
        ~is_exn_handler:is_exn_handler
    in
    let delayed_handlers =
      Delayed_handlers.union delayed_handlers_handler delayed_handlers_body
    in
    let delayed_handlers =
      Delayed_handlers.add_handler delayed_handlers name handler
    in
    if still_at_toplevel then
      drop_all_handlers delayed_handlers ~around:body, Delayed_handlers.empty
    else
      body, delayed_handlers
  | Apply { kind; func; args; continuation; exn_continuation;
      loc; should_be_tailcall = _; inlined; specialised = _; } ->
    let call_kind =
      match kind with
      | Function -> Call_kind.indirect_function_call_unknown_arity ()
      | Method { kind; obj; } ->
        Call_kind.method_call (LC.method_kind kind)
          ~obj:(find_simple t env obj)
    in
    let exn_continuation = close_exn_continuation t env exn_continuation in
    let apply =
      Flambda.Apply.create ~callee:(find_simple_from_id t env func)
        ~continuation:(Return continuation)
        exn_continuation
        ~args:(find_simples t env args)
        ~call_kind
        (Debuginfo.from_location loc)
        ~inline:(LC.inline_attribute inlined)
        ~inlining_depth:0
    in
    Expr.create_apply apply, Delayed_handlers.empty
  | Apply_cont (cont, trap_action, args) ->
    let args = find_simples t env args in
    let trap_action = close_trap_action_opt trap_action in
    let apply_cont =
      Flambda.Apply_cont.create ?trap_action cont ~args ~dbg:Debuginfo.none
    in
    Flambda.Expr.create_apply_cont apply_cont, Delayed_handlers.empty
  | Switch (scrutinee, sw) ->
    let scrutinee = Simple.name (Env.find_name env scrutinee) in
    let untagged_scrutinee = Variable.create "untagged" in
    let untagged_scrutinee' =
      VB.create untagged_scrutinee Name_mode.normal
    in
    let untag =
      Named.create_prim
        (Unary (Unbox_number Untagged_immediate, scrutinee))
        Debuginfo.none
    in
    let arms =
      List.map (fun (case, cont, trap_action, args) ->
          let trap_action = close_trap_action_opt trap_action in
          let args = find_simples t env args in
          Target_imm.int (Targetint.OCaml.of_int case),
            Flambda.Apply_cont.create ?trap_action cont ~args
              ~dbg:Debuginfo.none)
        sw.consts
    in
    match arms, sw.failaction with
    | [case, action], Some (default_action, default_trap_action, default_args)
        when sw.numconsts >= 3 ->
      (* Avoid enormous switches, where every arm goes to the same place
         except one, that arise from single-arm [Lambda] switches with a
         default case.  (Seen in code generated by ppx_compare for variants,
         which exhibited quadratic size blowup.) *)
      let compare =
        Named.create_prim
          (Binary (Phys_equal (Flambda_kind.naked_immediate, Eq),
            Simple.var untagged_scrutinee,
            Simple.const (Reg_width_const.naked_immediate case)))
          Debuginfo.none
      in
      let comparison_result = Variable.create "eq" in
      let comparison_result' = VB.create comparison_result Name_mode.normal in
      let default_action =
        let args = find_simples t env default_args in
        let trap_action = close_trap_action_opt default_trap_action in
        Flambda.Apply_cont.create ?trap_action default_action ~args
          ~dbg:Debuginfo.none
      in
      let switch =
        let arms =
          Target_imm.Map.singleton Target_imm.bool_false default_action
          |> Target_imm.Map.add Target_imm.bool_true action
        in
        Expr.create_switch ~scrutinee:(Simple.var comparison_result) ~arms
      in
      Expr.create_let untagged_scrutinee' untag
        (Expr.create_let comparison_result' compare switch),
      Delayed_handlers.empty
    | _, _ ->
      let arms =
        match sw.failaction with
        | None -> Target_imm.Map.of_list arms
        | Some (default, trap_action, args) ->
          Numbers.Int.Set.fold (fun case cases ->
              let case = Target_imm.int (Targetint.OCaml.of_int case) in
              if Target_imm.Map.mem case cases then cases
              else
                let args = find_simples t env args in
                let trap_action = close_trap_action_opt trap_action in
                let default =
                  Flambda.Apply_cont.create ?trap_action default ~args
                    ~dbg:Debuginfo.none
                in
                Target_imm.Map.add case default cases)
            (Numbers.Int.zero_to_n (sw.numconsts - 1))
            (Target_imm.Map.of_list arms)
      in
      if Target_imm.Map.is_empty arms then
        Expr.create_invalid (), Delayed_handlers.empty
      else
        Expr.create_let untagged_scrutinee' untag
          (Expr.create_switch ~scrutinee:(Simple.var untagged_scrutinee) ~arms),
        Delayed_handlers.empty

and close_named t env ~let_bound_var (named : Ilambda.named)
      (k : Named.t option -> Expr.t * _) : Expr.t * _ =
  match named with
  | Simple (Var id) ->
    let simple =
      if not (Ident.is_predef id) then Simple.var (Env.find_var env id)
      else symbol_for_ident t id
    in
    k (Some (Named.create_simple simple))
  | Simple (Const cst) ->
    let named, _name = close_const t cst in
    k (Some named)
  | Prim { prim; args; loc; exn_continuation; } ->
    close_primitive t env ~let_bound_var named prim ~args loc
      exn_continuation k
  | Assign _ ->
    Misc.fatal_error "[Assign] should have been removed by \
      [Eliminate_mutable_vars]"

and close_let_rec t env ~defs ~body =
  let env =
    List.fold_right (fun (id, _) env ->
        let env, _var = Env.add_var_like env id User_visible in
        env)
      defs env
  in
  let recursive_functions = Ilambda.recursive_functions defs in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let function_declarations =
    List.map (function (let_rec_ident,
            ({ kind; return_continuation; exn_continuation;
               params; return; body; free_idents_of_body;
               attr; loc; stub;
             } : Ilambda.function_declaration)) ->
        let closure_id =
          Closure_id.wrap compilation_unit
            (Variable.create_with_same_name_as_ident let_rec_ident)
        in
        let recursive : Recursive.t =
          if Ident.Set.mem let_rec_ident recursive_functions then
            Recursive
          else
            Non_recursive
        in
        let function_declaration =
          Function_decl.create ~let_rec_ident:(Some let_rec_ident)
            ~closure_id ~kind ~params ~return ~return_continuation
            ~exn_continuation ~body ~attr ~loc ~free_idents_of_body ~stub
            recursive
        in
        function_declaration)
      defs
  in
  let closure_vars =
    List.fold_left (fun closure_vars decl ->
        let closure_var =
          VB.create (Env.find_var env (Function_decl.let_rec_ident decl))
            Name_mode.normal
        in
        let closure_id = Function_decl.closure_id decl in
        Closure_id.Map.add closure_id closure_var closure_vars)
      Closure_id.Map.empty
      function_declarations
  in
  let set_of_closures =
    close_functions t env (Function_decls.create function_declarations)
  in
  (* CR mshinwell: We should maybe have something more elegant here *)
  let generated_closures =
    Closure_id.Set.diff
      (Closure_id.Map.keys (Flambda.Function_declarations.funs (
        Flambda.Set_of_closures.function_decls set_of_closures)))
      (Closure_id.Map.keys closure_vars)
  in
  let closure_vars =
    Closure_id.Set.fold (fun closure_id closure_vars ->
        let closure_var =
          VB.create (Variable.create "generated") Name_mode.normal
        in
        Closure_id.Map.add closure_id closure_var closure_vars)
      generated_closures
      closure_vars
  in
  let body, handlers = close t env body in
  let leaving_scope_of =
    Closure_id.Map.data closure_vars
    |> List.map VB.var
    |> Variable.Set.of_list
    |> Name_occurrences.create_variables' Name_mode.normal
  in
  let body, handlers =
    drop_handlers handlers ~leaving_scope_of ~around:body
  in
  let expr =
    Expr.create_pattern_let
      (Bindable_let_bound.set_of_closures ~closure_vars)
      (Flambda.Named.create_set_of_closures set_of_closures)
      body
  in
  expr, handlers

and close_functions t external_env function_declarations =
  let compilation_unit = Compilation_unit.get_current_exn () in
  let var_within_closures_from_idents =
    Ident.Set.fold (fun id map ->
        (* Filter out predefined exception identifiers, since they will be
           turned into symbols when we closure-convert the body. *)
        if Ident.is_predef id then map
        else
          let var = Variable.create_with_same_name_as_ident id in
          Ident.Map.add id (Var_within_closure.wrap compilation_unit var) map)
      (Function_decls.all_free_idents function_declarations)
      Ident.Map.empty
  in
  let func_decl_list = Function_decls.to_list function_declarations in
  let closure_ids_from_idents =
    List.fold_left (fun map decl ->
        let id = Function_decl.let_rec_ident decl in
        let closure_id = Function_decl.closure_id decl in
        Ident.Map.add id closure_id map)
      Ident.Map.empty
      func_decl_list
  in
  let funs =
    List.fold_left (fun by_closure_id function_decl ->
        close_one_function t ~external_env ~by_closure_id function_decl
          ~var_within_closures_from_idents ~closure_ids_from_idents
          function_declarations)
      Closure_id.Map.empty
      func_decl_list
  in
  (* CR lmaurer: funs has arbitrary order (ultimately coming from
     function_declarations) *)
  let funs =
    Closure_id.Lmap.of_list (Closure_id.Map.bindings funs)
  in
  let function_decls = Flambda.Function_declarations.create funs in
  let closure_elements =
    Ident.Map.fold (fun id var_within_closure map ->
        let external_var = Simple.var (Env.find_var external_env id) in
        Var_within_closure.Map.add var_within_closure external_var map)
      var_within_closures_from_idents
      Var_within_closure.Map.empty
  in
  Flambda.Set_of_closures.create function_decls ~closure_elements

and close_one_function t ~external_env ~by_closure_id decl
      ~var_within_closures_from_idents ~closure_ids_from_idents
      function_declarations =
  let body = Function_decl.body decl in
  let loc = Function_decl.loc decl in
  let dbg = Debuginfo.from_location loc in
  let params = Function_decl.params decl in
  let return = Function_decl.return decl in
  let recursive = Function_decl.recursive decl in
  let my_closure = Variable.create "my_closure" in
  let closure_id = Function_decl.closure_id decl in
  let our_let_rec_ident = Function_decl.let_rec_ident decl in
  let contains_closures = Function_decl.contains_closures decl in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let code_id =
    Code_id.create ~name:(Closure_id.to_string closure_id) compilation_unit
  in
  let unboxed_version =
    Closure_id.wrap compilation_unit (Variable.create (
      Ident.name (Function_decl.let_rec_ident decl)))
  in
  let my_closure_id, is_curried =
    match Function_decl.kind decl with
    | Curried -> closure_id, true
    | Tupled -> unboxed_version, false
  in
  (* The free variables are:
     - The parameters: direct substitution by [Variable]s
     - The function being defined: accessible through [my_closure]
     - Other functions in the set being defined: accessible from [my_closure]
       then a [Select_closure]
     - Other free variables: accessible using [Project_var] from
       [my_closure].
     Note that free variables corresponding to predefined exception
     identifiers have been filtered out by [close_functions], above.
  *)
  let var_within_closures_to_bind, var_within_closures_for_idents =
    Ident.Map.fold
      (fun id var_within_closures_for_idents (to_bind, var_for_ident) ->
        let var = Variable.create_with_same_name_as_ident id in
        Variable.Map.add var var_within_closures_for_idents to_bind,
          Ident.Map.add id var var_for_ident)
      var_within_closures_from_idents
      (Variable.Map.empty, Ident.Map.empty)
  in
  (* CR mshinwell: Remove "project_closure" names *)
  let project_closure_to_bind, vars_for_project_closure =
    List.fold_left (fun (to_bind, vars_for_idents) function_decl ->
        let let_rec_ident = Function_decl.let_rec_ident function_decl in
        let to_bind, var =
          if Ident.same our_let_rec_ident let_rec_ident && is_curried then
            (* When the function being compiled is tupled, my_closure
               points to the curried version but let_rec_ident is called
               with tuple arguments, so the correct closure to bind
               is the one in the closure_ids_from_idents map.
            *)
            to_bind, my_closure  (* my_closure is already bound *)
          else
            let variable =
              Variable.create_with_same_name_as_ident let_rec_ident
            in
            let closure_id =
              Ident.Map.find let_rec_ident closure_ids_from_idents
            in
            Variable.Map.add variable closure_id to_bind, variable
        in
        to_bind,
        Ident.Map.add let_rec_ident var vars_for_idents)
      (Variable.Map.empty, Ident.Map.empty)
      (Function_decls.to_list function_declarations)
  in
  let closure_env_without_parameters =
    let empty_env = Env.clear_local_bindings external_env in
    Env.add_var_map (Env.add_var_map empty_env var_within_closures_for_idents)
      vars_for_project_closure
  in
  let closure_env =
    List.fold_right (fun (id, _) env ->
        let env, _var = Env.add_var_like env id User_visible in
        env)
      params
      closure_env_without_parameters
  in
  (* CR-someday pchambart: eta-expansion wrappers for primitives are
     not marked as stubs but certainly should be. *)
  let stub = Function_decl.stub decl in
  let param_vars =
    List.map (fun (id, kind) -> Env.find_var closure_env id, kind) params
  in
  let params =
    List.map (fun (var, kind) ->
        Kinded_parameter.create var (LC.value_kind kind))
      param_vars
  in
  let body, handlers =
    try close t closure_env body
    with Misc.Fatal_error -> begin
      if !Clflags.flambda_context_on_error then begin
        Format.eprintf "\n%sContext is:%s closure converting \
          function@ with [our_let_rec_ident] %a (closure ID %a)@ \
          and body:@ %a"
          (Flambda_colours.error ())
          (Flambda_colours.normal ())
          Ident.print our_let_rec_ident
          Closure_id.print closure_id
          Ilambda.print body
      end;
      raise Misc.Fatal_error
    end
  in
  let body = drop_all_handlers handlers ~around:body in
  let free_names_of_body = Expr.free_names body in
  let my_closure' = Simple.var my_closure in
  let body =
    Variable.Map.fold (fun var closure_id body ->
        if not (Name_occurrences.mem_var free_names_of_body var) then body
        else
          let move : Flambda_primitive.unary_primitive =
            Select_closure {
              move_from = my_closure_id;
              move_to = closure_id;
            }
          in
          let var = VB.create var Name_mode.normal in
          Expr.create_let var
            (Named.create_prim (Unary (move, my_closure')) Debuginfo.none)
            body)
      project_closure_to_bind
      body
  in
  let body =
    Variable.Map.fold (fun var var_within_closure body ->
        if not (Name_occurrences.mem_var free_names_of_body var) then body
        else
          let var = VB.create var Name_mode.normal in
          Expr.create_let var
            (Named.create_prim
              (Unary (Project_var {
                 project_from = my_closure_id;
                 var = var_within_closure;
               }, my_closure'))
              Debuginfo.none)
            body)
      var_within_closures_to_bind
      body
  in
  let exn_continuation =
    close_exn_continuation t external_env (Function_decl.exn_continuation decl)
  in
  let inline : Inline_attribute.t =
    (* We make a decision based on [fallback_inlining_heuristic] here to try
       to mimic Closure's behaviour as closely as possible, particularly
       when there are functions involving constant closures, which are not
       lifted during Closure (but will prevent inlining) but will likely have
       been lifted by our other check in [Inlining_cost] (thus preventing us
       seeing they were originally there). *)
    if contains_closures
      && !Clflags.Flambda.Expert.fallback_inlining_heuristic
    then Never_inline
    else LC.inline_attribute (Function_decl.inline decl)
  in
  let params_and_body =
    Flambda.Function_params_and_body.create
      ~return_continuation:(Function_decl.return_continuation decl)
      exn_continuation params ~dbg ~body ~my_closure
  in
  let fun_decl =
    Flambda.Function_declaration.create ~code_id
      ~params_arity:(Kinded_parameter.List.arity params)
      ~result_arity:[LC.value_kind return]
      ~stub
      ~dbg
      ~inline
      ~is_a_functor:(Function_decl.is_a_functor decl)
      ~recursive
  in
  t.code <- (code_id, params_and_body) :: t.code;
  match Function_decl.kind decl with
  | Curried -> Closure_id.Map.add my_closure_id fun_decl by_closure_id
  | Tupled ->
    let generic_function_stub, code_id, params_and_body =
      tupled_function_call_stub param_vars unboxed_version code_id ~closure_id
        recursive
    in
    t.code <- (code_id, params_and_body) :: t.code;
    Closure_id.Map.add unboxed_version fun_decl
      (Closure_id.Map.add closure_id generic_function_stub by_closure_id)

let ilambda_to_flambda ~backend ~module_ident ~module_block_size_in_words
      ~filename (ilam : Ilambda.program) =
  let module Backend = (val backend : Flambda_backend_intf.S) in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let bound_error_symbol =
    Lambda_to_flambda_primitives_helpers.caml_ml_array_bound_error
  in
  let t =
    { backend;
      current_unit_id = Compilation_unit.get_persistent_ident compilation_unit;
      symbol_for_global' = Backend.symbol_for_global';
      filename;
      (* CR: externals such as bound_error_symbol should ideally not be par of
         the imported symbols, and not be required to be in the typing env. *)
      imported_symbols = Symbol.Set.singleton bound_error_symbol;
      declared_symbols = [];
      shareable_constants = Static_const.Map.empty;
      code = [];
      ilambda_exn_continuation = ilam.exn_continuation.exn_handler;
    }
  in
  let module_symbol =
    Backend.symbol_for_global' (
      Ident.create_persistent (Ident.name module_ident))
  in
  let module_block_tag = Tag.Scannable.zero in
  let module_block_var = Variable.create "module_block" in
  let return_cont = Continuation.create ~sort:Toplevel_return () in
  let load_fields_body =
    let field_vars =
      List.init module_block_size_in_words (fun pos ->
        let pos_str = string_of_int pos in
        pos, Variable.create ("field_" ^ pos_str))
    in
    let body =
      let static_const : Static_const.t =
        let field_vars =
          List.map (fun (_, var) : Static_const.Field_of_block.t ->
              Dynamically_computed var)
            field_vars
        in
        Block (module_block_tag, Immutable, field_vars)
      in
      let return =
        (* Module initialisers return unit, but since that is taken care of
           during Cmm generation, we can instead "return" [module_symbol]
           here to ensure that its associated [Let_symbol] doesn't get
           deleted. *)
        Flambda.Apply_cont.create return_cont
          ~args:[Simple.symbol module_symbol]
          ~dbg:Debuginfo.none
        |> Expr.create_apply_cont
      in
      Let_symbol.create Syntactic (Singleton module_symbol) static_const return
      |> Flambda.Expr.create_let_symbol
    in
    let block_access : P.Block_access_kind.t =
      Values {
        tag = Tag.Scannable.zero;
        size = Known (Targetint.OCaml.of_int module_block_size_in_words);
        field_kind = Any_value;
      }
    in
    List.fold_left (fun body (pos, var) ->
        let var = VB.create var Name_mode.normal in
        let pos = Target_imm.int (Targetint.OCaml.of_int pos) in
        Expr.create_let var
          (Named.create_prim
            (Binary (
              Block_load (block_access, Immutable),
              Simple.var module_block_var,
              Simple.const (Reg_width_const.tagged_immediate pos)))
            Debuginfo.none)
          body)
      body (List.rev field_vars)
  in
  let load_fields_cont_handler =
    let param = Kinded_parameter.create module_block_var K.value in
    let params_and_handler =
      Flambda.Continuation_params_and_handler.create [param]
        ~handler:load_fields_body;
    in
    Flambda.Continuation_handler.create ~params_and_handler
      ~stub:false  (* CR mshinwell: remove "stub" notion *)
      ~is_exn_handler:false
  in
  let body =
    (* This binds the return continuation that is free (or, at least, not bound)
       in the incoming Ilambda code. The handler for the continuation receives a
       tuple with fields indexed from zero to [module_block_size_in_words]. The
       handler extracts the fields; the variables bound to such fields are then
       used to define the module block symbol. *)
    let body, handlers = close t Env.empty ilam.expr in
    let body = drop_all_handlers handlers ~around:body in
    Flambda.Let_cont.create_non_recursive ilam.return_continuation
      load_fields_cont_handler
      ~body
  in
  begin match ilam.exn_continuation.extra_args with
  | [] -> ()
  | _::_ ->
    Misc.fatal_error "Ilambda toplevel exception continuation cannot have \
      extra arguments"
  end;
  let exn_continuation = ilam.exn_continuation.exn_handler in
  let body =
    List.fold_left (fun body (code_id, params_and_body) ->
        let bound_symbols : Let_symbol.Bound_symbols.t =
          Sets_of_closures [{
            code_ids = Code_id.Set.singleton code_id;
            closure_symbols = Closure_id.Lmap.empty;
          }]
        in
        let static_const : Static_const.t =
          let code : Static_const.Code.t =
            { params_and_body = Present params_and_body;
              newer_version_of = None;
            }
          in
          Sets_of_closures [{
            code = Code_id.Lmap.singleton code_id code;
            set_of_closures = Set_of_closures.empty;
          }]
        in
        Let_symbol.create Syntactic bound_symbols static_const body
        |> Flambda.Expr.create_let_symbol)
      body
      t.code
  in
  (* We must make sure there is always an outer [Let_symbol] binding so that
     lifted constants not in the scope of any other [Let_symbol] binding get
     put into the term and not dropped.  Adding this extra binding, which
     will actually be removed by the simplifier, avoids a special case. *)
  begin match t.declared_symbols with
  | _::_ -> ()
  | [] ->
    let (_sym : Symbol.t) =
      register_const0 t
        (Static_const.Block (Tag.Scannable.zero, Immutable, []))
        "first_const"
    in
    ()
  end;
  let body =
    List.fold_left (fun body (symbol, static_const) ->
        Let_symbol.create Syntactic (Singleton symbol) static_const body
        |> Flambda.Expr.create_let_symbol)
      body
      t.declared_symbols
  in
  (* CR mshinwell: Delete [t.imported_symbols] if unused *)
  Flambda_unit.create ~return_continuation:return_cont ~exn_continuation ~body
    ~module_symbol
