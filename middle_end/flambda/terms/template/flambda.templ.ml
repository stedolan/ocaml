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
(* CR mshinwell: Fix warning 60! *)
[@@@ocaml.warning "-60"]
[@@@ocaml.warning "-32"]

module K = Flambda_kind
module KP = Kinded_parameter

module Apply = Apply_expr
module Apply_cont = Apply_cont_expr
module Switch = Switch_expr

let fprintf = Format.fprintf

(* -- module rec binding here -- *)

(* CR mshinwell: Consider counting numbers of names in Name_occurrences *)
(* CR mshinwell: Check that apply_cont is well-formed when there is a
   trap installation or removal. *)
(* CR-someday pchambart: for sum types, we should probably add an exhaustive
   pattern in ignores functions to be reminded if a type change *)
(* CR-someday mshinwell: We should make "direct applications should not have
   overapplication" be an invariant throughout.  At the moment I think this is
   only true after [Simplify] has split overapplications. *)

module Function_declaration = Function_declaration
module Function_declarations = Function_declarations
module Let = Let_expr
module Let_cont = Let_cont_expr
module Set_of_closures = Set_of_closures

module Import = struct
  module Apply = Apply
  module Apply_cont = Apply_cont
  module Continuation_handler = Continuation_handler
  module Continuation_handlers = Continuation_handlers
  module Continuation_params_and_handler = Continuation_params_and_handler
  module Expr = Expr
  module Function_declaration = Function_declaration
  module Function_declarations = Function_declarations
  module Function_params_and_body = Function_params_and_body
  module Let = Let
  module Let_cont = Let_cont
  module Named = Named
  module Non_recursive_let_cont_handler = Non_recursive_let_cont_handler
  module Recursive_let_cont_handlers = Recursive_let_cont_handlers
  module Set_of_closures = Set_of_closures
  module Static_const = Static_const
  module Switch = Switch
end

(* CR mshinwell: Add a type to just encapsulate bound_symbols and
   defining_expr.  (Maybe this can just use [Lifted_constant] now?  These
   functions would have to move e.g. to [Simplify_common].) *)
let pieces_of_code ?newer_versions_of ?set_of_closures code =
  let newer_versions_of =
    Option.value newer_versions_of ~default:Code_id.Map.empty
  in
  let code =
    Code_id.Lmap.mapi (fun id params_and_body : Static_const.Code.t ->
        let newer_version_of =
          Code_id.Map.find_opt id newer_versions_of
        in
        { params_and_body = Present params_and_body;
          newer_version_of;
        })
      code
  in
  let closure_symbols, set_of_closures =
    Option.value set_of_closures
      ~default:(Closure_id.Lmap.empty, Set_of_closures.empty)
  in
  let static_const : Static_const.t =
    Sets_of_closures [{
      code;
      set_of_closures;
    }]
  in
  let bound_symbols : Bound_symbols.t =
    Sets_of_closures [{
      code_ids = Code_id.Lmap.keys code |> Code_id.Set.of_list;
      closure_symbols;
    }]
  in
  bound_symbols, static_const

let deleted_pieces_of_code ?newer_versions_of code_ids =
  let newer_versions_of =
    Option.value newer_versions_of ~default:Code_id.Map.empty
  in
  let code =
    Code_id.Set.fold (fun id code_map ->
        let newer_version_of =
          Code_id.Map.find_opt id newer_versions_of
        in
        let code : Static_const.Code.t =
          { params_and_body = Deleted;
            newer_version_of;
          }
        in
        Code_id.Lmap.add id code code_map)
      code_ids
      Code_id.Lmap.empty
  in
  let static_const : Static_const.t =
    Sets_of_closures [{
      code;
      set_of_closures = Set_of_closures.empty;
    }]
  in
  let bound_symbols : Bound_symbols.t =
    Sets_of_closures [{
      code_ids = Code_id.Lmap.keys code |> Code_id.Set.of_list;
      closure_symbols = Closure_id.Lmap.empty;
    }]
  in
  bound_symbols, static_const
