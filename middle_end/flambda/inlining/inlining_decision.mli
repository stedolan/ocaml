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

open! Flambda.Import

module Function_declaration_decision : sig
  type t = private
    | Never_inline_attribute
    | Function_body_too_large
    | Stub
    | Inline

  val can_inline : t -> bool
end

val make_decision_for_function_declaration
   : Simplify_env_and_result.Downwards_env.t
  -> Function_declaration.t
  -> Function_declaration_decision.t

module Call_site_decision : sig
  type attribute_causing_inlining = private
    | Unroll
    | Always

  type t = private
    | Environment_says_never_inline
    | Unrolling_depth_exceeded
    | Max_inlining_depth_exceeded
    | Recursion_depth_exceeded
    | Never_inline_attribute
    | Inline of {
        attribute : attribute_causing_inlining option;
        unroll_to : int option;
      }

  val print : Format.formatter -> t -> unit

  type can_inline = private
    | Do_not_inline
    | Inline of { unroll_to : int option; }

  val can_inline : t -> can_inline
end

val make_decision_for_call_site
   : Simplify_env_and_result.Downwards_env.t
  -> function_decl_rec_info:Rec_info.t
  -> apply_inlining_depth:int
  -> Inline_attribute.t
  -> Call_site_decision.t
