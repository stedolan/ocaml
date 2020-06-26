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

module DA = Downwards_acc
module DE = Simplify_env_and_result.Downwards_env
module I = Flambda_type.Function_declaration_type.Inlinable
module VB = Var_in_binding_pos

let inline dacc ~callee ~args function_decl
      ~apply_return_continuation ~apply_exn_continuation
      ~apply_inlining_depth ~unroll_to dbg =
  let denv = DA.denv dacc in
  let params_and_body = DE.find_code denv (I.code_id function_decl) in
  Function_params_and_body.pattern_match params_and_body
    ~f:(fun ~return_continuation exn_continuation params ~body ~my_closure ->
          let denv =
            DE.set_inlining_depth_increment
              (DE.set_inlined_debuginfo denv dbg)
              (apply_inlining_depth + 1)
          in
          let make_inlined_body ~apply_exn_continuation ~apply_return_continuation =
            let perm = Name_permutation.empty in
            let perm =
              match (apply_return_continuation : Apply.Result_continuation.t)with
              | Return k ->
                Name_permutation.add_continuation perm
                return_continuation k
              | Never_returns -> perm
            in
            let perm =
              Name_permutation.add_continuation perm
                (Exn_continuation.exn_handler exn_continuation)
                apply_exn_continuation
            in
            let callee =
              Simple.merge_rec_info callee
                ~newer_rec_info:(Some (Rec_info.create ~depth:1 ~unroll_to))
              |> Option.get  (* CR mshinwell: improve *)
            in
            Expr.apply_name_permutation
              (Expr.bind_parameters_to_simples ~bind:params ~target:args
                (Expr.create_let
                  (VB.create my_closure Name_mode.normal)
                  (Named.create_simple callee)
                  body))
              perm
          in
          let expr =
            assert (Exn_continuation.extra_args exn_continuation = []);
            match Exn_continuation.extra_args apply_exn_continuation with
            | [] ->
              make_inlined_body ~apply_exn_continuation:
                (Exn_continuation.exn_handler apply_exn_continuation)
                ~apply_return_continuation
            | extra_args ->
              (* We need to add a wrapper for the exception handler, so that
                 exceptions coming from the inlined body go through the wrapper
                 and are re-raised with the correct extra arguments.
                 This means we also need to add a push trap before the inlined
                 body, and a pop trap after.
                 The push trap is simply a matter of jumping to the body, while
                 the pop trap needs to replace the body's return continuation with
                 a wrapper that pops then jumps back.
              *)
              let wrapper = Continuation.create ~sort:Exn () in
              let body_with_pop =
                match (apply_return_continuation : Apply.Result_continuation.t) with
                | Never_returns ->
                  make_inlined_body ~apply_exn_continuation:wrapper
                    ~apply_return_continuation
                | Return apply_return_continuation ->
                  let pop_wrapper_cont = Continuation.create () in
                  let pop_wrapper_handler =
                    let kinded_params =
                      List.map (fun k -> (Variable.create "wrapper_return", k))
                        (I.result_arity function_decl)
                    in
                    let trap_action =
                      Trap_action.Pop { exn_handler = wrapper; raise_kind = None; }
                    in
                    let args = List.map (fun (v, _) -> Simple.var v) kinded_params in
                    let handler =
                      Expr.create_apply_cont (Apply_cont.create
                                                ~trap_action
                                                apply_return_continuation
                                                ~args
                                                ~dbg: Debuginfo.none)
                    in
                    let params_and_handler =
                      Continuation_params_and_handler.create
                        (Kinded_parameter.List.create kinded_params)
                        ~handler
                    in
                    Continuation_handler.create ~params_and_handler
                      ~stub:false
                      ~is_exn_handler:false
                  in
                  let body =
                    make_inlined_body ~apply_exn_continuation:wrapper
                      ~apply_return_continuation:(Return pop_wrapper_cont)
                  in
                  Let_cont.create_non_recursive pop_wrapper_cont
                    pop_wrapper_handler ~body
              in
              let wrapper_handler =
                let param = Variable.create "exn" in
                let kinded_params = [KP.create param K.value] in
                let exn_handler =
                  Exn_continuation.exn_handler apply_exn_continuation
                in
                let trap_action =
                  Trap_action.Pop { exn_handler; raise_kind = None; }
                in
                let handler =
                  Expr.create_apply_cont (Apply_cont.create
                    ~trap_action
                    (Exn_continuation.exn_handler apply_exn_continuation)
                    ~args:((Simple.var param) :: (List.map fst extra_args))
                    ~dbg:Debuginfo.none) (* Backtrace building functions expect
                                            compiler-generated raises not to
                                            have any debug info *)
                in
                let params_and_handler =
                  Continuation_params_and_handler.create kinded_params ~handler
                in
                Continuation_handler.create ~params_and_handler
                  ~stub:false
                  ~is_exn_handler:true
              in
              let body_with_push =
                (* Wrap the body between push and pop of the wrapper handler *)
                let push_wrapper_cont = Continuation.create () in
                let handler = body_with_pop in
                let params_and_handler =
                  Continuation_params_and_handler.create [] ~handler
                in
                let push_wrapper_handler =
                  Continuation_handler.create ~params_and_handler
                    ~stub:false
                    ~is_exn_handler:false
                in
                let trap_action =
                  Trap_action.Push { exn_handler = wrapper; }
                in
                let body =
                  Expr.create_apply_cont (Apply_cont.create
                    ~trap_action
                    push_wrapper_cont
                    ~args: []
                    ~dbg: Debuginfo.none)
                in
                Let_cont.create_non_recursive push_wrapper_cont
                  push_wrapper_handler ~body
              in
              Let_cont.create_non_recursive wrapper wrapper_handler
                ~body:body_with_push
          in
(*
  Format.eprintf "Inlined body to be simplified:@ %a\n%!"
    Expr.print expr;
*)
(*
  Format.eprintf "Inlined body to be simplified:@ %a@ dacc:@ %a\n%!"
    Expr.print expr DA.print dacc;
*)
          DA.with_denv dacc denv, expr)
