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

module Apply = Flambda.Apply
module Apply_cont = Flambda.Apply_cont
module Continuation_handler = Flambda.Continuation_handler
module Continuation_handlers = Flambda.Continuation_handlers
module Continuation_params_and_handler = Flambda.Continuation_params_and_handler
module Expr = Flambda.Expr
module Function_declaration = Flambda.Function_declaration
module Function_declarations = Flambda.Function_declarations
module Function_params_and_body = Flambda.Function_params_and_body
module Let_cont = Flambda.Let_cont
module Let = Flambda.Let
module Lifted_constant = Simplify_env_and_result.Lifted_constant
module Lifted_constant_state = Simplify_env_and_result.Lifted_constant_state
module Named = Flambda.Named
module Non_recursive_let_cont_handler = Flambda.Non_recursive_let_cont_handler
module Recursive_let_cont_handlers = Flambda.Recursive_let_cont_handlers
module Set_of_closures = Flambda.Set_of_closures
module Static_const = Flambda.Static_const
module Switch = Flambda.Switch

module CH = Continuation_handler
module CIS = Code_id_or_symbol
module CPH = Continuation_params_and_handler
module CUE = Continuation_uses_env
module DA = Downwards_acc
module DE = Simplify_env_and_result.Downwards_env
module EA = Continuation_extra_params_and_args.Extra_arg
module EPA = Continuation_extra_params_and_args
module FD = Function_declaration
module FU = Flambda_unit
module K = Flambda_kind
module KP = Kinded_parameter
module LC = Simplify_env_and_result.Lifted_constant
module LCS = Simplify_env_and_result.Lifted_constant_state
module NM = Name_mode
module P = Flambda_primitive
module R = Simplify_env_and_result.Result
module S = Simplify_simple
module SC = Static_const
module T = Flambda_type
module TE = Flambda_type.Typing_env
module TEE = Flambda_type.Typing_env_extension
module UA = Upwards_acc
module UE = Simplify_env_and_result.Upwards_env
module VB = Var_in_binding_pos
