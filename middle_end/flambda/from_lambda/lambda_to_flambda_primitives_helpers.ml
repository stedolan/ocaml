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

open! Flambda.Import

module P = Flambda_primitive
module VB = Var_in_binding_pos

(* May be useful for compiling out bounds checks:
type bounds_check_result =
  | In_range
  | Out_of_range

let bounds_check ~width ~string_length_in_bytes ~index_in_bytes
      : bounds_check_result =
  let index_in_bytes = Target_imm.to_targetint index_in_bytes in
  if Targetint.OCaml.compare index_in_bytes Targetint.OCaml.zero < 0 then
    Out_of_range
  else
    let result_size_in_bytes =
      Targetint.OCaml.of_int
        (Flambda_primitive.byte_width_of_string_accessor_width width)
    in
    (* We are careful here to avoid overflow for ease of reasoning. *)
    let highest_index_allowed =
      Targetint.OCaml.sub string_length_in_bytes result_size_in_bytes
    in
    if Targetint.OCaml.compare index_in_bytes highest_index_allowed >= 0 then
      Out_of_range
    else
      In_range
*)

type failure =
  | Division_by_zero
  | Index_out_of_bounds

type expr_primitive =
  | Simple of Simple.t
  | Unary of P.unary_primitive * simple_or_prim
  | Binary of P.binary_primitive * simple_or_prim * simple_or_prim
  | Ternary of P.ternary_primitive * simple_or_prim * simple_or_prim
      * simple_or_prim
  | Variadic of P.variadic_primitive * (simple_or_prim list)
  | Checked of { validity_conditions : expr_primitive list;
                 primitive : expr_primitive;
                 failure : failure; (* Predefined exception *)
                 dbg : Debuginfo.t }

and simple_or_prim =
  | Simple of Simple.t
  | Prim of expr_primitive

let rec print_expr_primitive ppf expr_primitive =
  let module W = Flambda_primitive.Without_args in
  match expr_primitive with
  | Simple simple -> Simple.print ppf simple
  | Unary (prim, _) -> W.print ppf (Unary prim)
  | Binary (prim, _, _) -> W.print ppf (Binary prim)
  | Ternary (prim, _, _, _) -> W.print ppf (Ternary prim)
  | Variadic (prim, _) -> W.print ppf (Variadic prim)
  | Checked { primitive; _ } ->
    Format.fprintf ppf "@[<hov 1>(Checked@ %a)@]"
      print_expr_primitive primitive

let print_simple_or_prim ppf (simple_or_prim : simple_or_prim) =
  match simple_or_prim with
  | Simple simple -> Simple.print ppf simple
  | Prim _ -> Format.pp_print_string ppf "<prim>"

let print_list_of_simple_or_prim ppf simple_or_prim_list =
  Format.fprintf ppf "@[(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space print_simple_or_prim)
    simple_or_prim_list

let caml_ml_array_bound_error =
  let name = Linkage_name.create "caml_ml_array_bound_error" in
  Symbol.create (Compilation_unit.external_symbols ()) name

let raise_exn_for_failure ~dbg exn_cont exn_bucket extra_let_binding =
  let exn_handler = Exn_continuation.exn_handler exn_cont in
  let trap_action =
    Trap_action.Pop {
      exn_handler;
      raise_kind = Some Regular;
    }
  in
  let args =
    let extra_args =
      List.map (fun (simple, _kind) -> simple)
        (Exn_continuation.extra_args exn_cont)
    in
    [exn_bucket] @ extra_args
  in
  let apply_cont =
    Expr.create_apply_cont
      (Apply_cont.create ~trap_action exn_handler ~args ~dbg)
  in
  match extra_let_binding with
  | None -> apply_cont
  | Some (bound_var, defining_expr) ->
    Expr.create_let bound_var defining_expr apply_cont

let expression_for_failure ~backend exn_cont ~register_const_string
      primitive dbg (failure : failure) =
  let module B = (val backend : Flambda_backend_intf.S) in
  let exn_cont =
    match exn_cont with
    | Some exn_cont -> exn_cont
    | None ->
      Misc.fatal_errorf "Validity checks for primitive@ %a@ may raise, but \
          no exception continuation was supplied with the Lambda primitive"
        print_expr_primitive primitive
  in
  match failure with
  | Division_by_zero ->
    raise_exn_for_failure ~dbg exn_cont
      (Simple.symbol B.division_by_zero) None
  | Index_out_of_bounds ->
    if true then begin
      let call =
        let callee = Simple.symbol caml_ml_array_bound_error in
        let continuation = Apply.Result_continuation.Never_returns in
        let args = [] in
        let call_kind = Call_kind.c_call
                          ~alloc:false
                          ~param_arity:[]
                          ~return_arity:[]
        in
        (* These inlining fields should not be used for C calls since
           they can't really be inlined anyway. *)
        let inline = Inline_attribute.Never_inline in
        let inlining_depth = 0 in
        Apply.create ~callee ~continuation exn_cont
          ~args ~call_kind dbg ~inline ~inlining_depth
      in
      Expr.create_apply call
    end else begin
      let exn_bucket = Variable.create "exn_bucket" in
      (* CR mshinwell: Share this text with elsewhere. *)
      let error_text = register_const_string "index out of bounds" in
      let contents_of_exn_bucket = [
        Simple.symbol B.invalid_argument;
        Simple.symbol error_text;
      ]
      in
      let extra_let_binding =
        Var_in_binding_pos.create exn_bucket Name_mode.normal,
        Named.create_prim (Variadic (Make_block (
          Values (Tag.Scannable.zero, [Any_value; Any_value]),
          Immutable),
                                     contents_of_exn_bucket))
          dbg
      in
      raise_exn_for_failure
        ~dbg exn_cont
        (Simple.var exn_bucket)
        (Some extra_let_binding)
    end

let rec bind_rec ~backend exn_cont
          ~register_const_string
          (prim : expr_primitive)
          (dbg : Debuginfo.t)
          (cont : Named.t -> Expr.t * _)
  : Expr.t * _ =
  match prim with
  | Simple simple -> cont (Named.create_simple simple)
  | Unary (prim, arg) ->
    let cont (arg : Simple.t) =
      cont (Named.create_prim (Unary (prim, arg)) dbg)
    in
    bind_rec_primitive ~backend exn_cont ~register_const_string arg dbg cont
  | Binary (prim, arg1, arg2) ->
    let cont (arg2 : Simple.t) =
      let cont (arg1 : Simple.t) =
        cont (Named.create_prim (Binary (prim, arg1, arg2)) dbg)
      in
      bind_rec_primitive ~backend exn_cont ~register_const_string arg1 dbg cont
    in
    bind_rec_primitive ~backend exn_cont ~register_const_string arg2 dbg cont
  | Ternary (prim, arg1, arg2, arg3) ->
    let cont (arg3 : Simple.t) =
      let cont (arg2 : Simple.t) =
        let cont (arg1 : Simple.t) =
          cont (Named.create_prim (Ternary (prim, arg1, arg2, arg3)) dbg)
        in
        bind_rec_primitive ~backend exn_cont ~register_const_string arg1
          dbg cont
      in
      bind_rec_primitive ~backend exn_cont ~register_const_string arg2 dbg cont
    in
    bind_rec_primitive ~backend exn_cont ~register_const_string arg3 dbg cont
  | Variadic (prim, args) ->
    let cont args =
      cont (Named.create_prim (Variadic (prim, args)) dbg)
    in
    let rec build_cont args_to_convert converted_args =
      match args_to_convert with
      | [] ->
        cont converted_args
      | arg :: args_to_convert ->
        let cont arg =
          build_cont args_to_convert (arg :: converted_args)
        in
        bind_rec_primitive ~backend exn_cont ~register_const_string arg dbg cont
    in
    build_cont (List.rev args) []
  | Checked { validity_conditions; primitive; failure; dbg; } ->
    let primitive_cont = Continuation.create () in
    let primitive_cont_handler, delayed_handlers =
      let handler, delayed_handlers =
        bind_rec ~backend exn_cont ~register_const_string
          primitive dbg cont
      in
      let params_and_handler =
        Continuation_params_and_handler.create [] ~handler
      in
      let cont_handler =
        Continuation_handler.create ~params_and_handler
          ~stub:false
          ~is_exn_handler:false
      in
      cont_handler, delayed_handlers
    in
    let failure_cont = Continuation.create () in
    let failure_cont_handler =
      let handler =
        expression_for_failure ~backend exn_cont
          ~register_const_string primitive dbg failure
      in
      let params_and_handler =
        Continuation_params_and_handler.create [] ~handler
      in
      Continuation_handler.create ~params_and_handler
        ~stub:false
        ~is_exn_handler:false
    in
    let check_validity_conditions, delayed_handlers =
      List.fold_left (fun (rest, delayed_handlers) expr_primitive ->
          let condition_passed_cont = Continuation.create () in
          let condition_passed_cont_handler =
            let params_and_handler =
              Continuation_params_and_handler.create [] ~handler:rest
            in
            Continuation_handler.create ~params_and_handler
              ~stub:false
              ~is_exn_handler:false
          in
          let body, delayed_handlers' =
            bind_rec_primitive ~backend exn_cont ~register_const_string
              (Prim expr_primitive) dbg
              (fun prim_result ->
                (Expr.create_switch
                  ~scrutinee:prim_result
                  ~arms:(Target_imm.Map.of_list [
                    Target_imm.bool_true,
                      Apply_cont.goto condition_passed_cont;
                    Target_imm.bool_false,
                      Apply_cont.goto failure_cont;
                  ])), Delayed_handlers.empty)
          in
          let body =
            Let_cont.create_non_recursive condition_passed_cont
              condition_passed_cont_handler ~body
          in
          body, Delayed_handlers.union delayed_handlers' delayed_handlers)
        (Expr.create_apply_cont
           (Apply_cont.create primitive_cont ~args:[] ~dbg:Debuginfo.none),
         delayed_handlers)
        validity_conditions
    in
    let expr =
      Let_cont.create_non_recursive primitive_cont
        primitive_cont_handler
        ~body:(
          Let_cont.create_non_recursive failure_cont
            failure_cont_handler
            ~body:check_validity_conditions)
    in
    expr, delayed_handlers

and bind_rec_primitive ~backend exn_cont ~register_const_string
      (prim : simple_or_prim)
      (dbg : Debuginfo.t)
      (cont : Simple.t -> Expr.t * _) : Expr.t * _ =
  match prim with
  | Simple s ->
    cont s
  | Prim p ->
    let var = Variable.create "prim" in
    let var' = VB.create var Name_mode.normal in
    let cont named =
      let body, delayed_handlers = cont (Simple.var var) in
      Flambda.Expr.create_let var' named body, delayed_handlers
    in
    bind_rec ~backend exn_cont ~register_const_string p dbg cont
