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

module T = Flambda_type
module VB = Var_in_binding_pos

module Bound_vars_and_body = Name_abstraction.Make (Bindable_let_bound) (Expr)

type t = {
  bound_vars_and_body : Bound_vars_and_body.t;
  defining_expr : Named.t;
}

let pattern_match t ~f =
  Bound_vars_and_body.pattern_match t.bound_vars_and_body
    ~f:(fun bound_vars body -> f ~bound_vars ~body)

let pattern_match_pair t1 t2 ~f =
  Bound_vars_and_body.pattern_match_pair
    t1.bound_vars_and_body
    t2.bound_vars_and_body
    ~f:(fun bound_vars body1 body2 -> f ~bound_vars ~body1 ~body2)

let print_with_cache ~cache ppf
      ({ bound_vars_and_body = _; defining_expr; } as t) =
  let let_bound_var_colour bound_vars =
    let kind = Bindable_let_bound.name_mode bound_vars in
    if Name_mode.is_phantom kind then
      Flambda_colours.elide ()
    else
      Flambda_colours.variable ()
  in
  let rec let_body (expr : Expr.t) =
    match Expr.descr expr with
    | Let ({ bound_vars_and_body = _; defining_expr; } as t) ->
      pattern_match t ~f:(fun ~bound_vars ~body ->
        fprintf ppf
          "@ @[<hov 1>@<0>%s%a@<0>%s =@<0>%s@ %a@]"
          (let_bound_var_colour bound_vars)
          Bindable_let_bound.print bound_vars
          (Flambda_colours.elide ())
          (Flambda_colours.normal ())
          (Named.print_with_cache ~cache) defining_expr;
        let_body body)
    | _ -> expr
  in
  pattern_match t ~f:(fun ~bound_vars ~body ->
    fprintf ppf "@[<v 1>(@<0>%slet@<0>%s@ (@[<v 0>\
        @[<hov 1>@<0>%s%a@<0>%s =@<0>%s@ %a@]"
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
      (let_bound_var_colour bound_vars)
      Bindable_let_bound.print bound_vars
      (Flambda_colours.elide ())
      (Flambda_colours.normal ())
      (Named.print_with_cache ~cache) defining_expr;
    let expr = let_body body in
    fprintf ppf "@])@ %a)@]"
      (Expr.print_with_cache ~cache) expr)

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let create ~bound_vars ~defining_expr ~body =
  let bound_vars_and_body = Bound_vars_and_body.create bound_vars body in
  { bound_vars_and_body;
    defining_expr;
  }

let invariant env t =
  let module E = Invariant_env in
  Named.invariant env t.defining_expr;
  pattern_match t ~f:(fun ~(bound_vars : Bindable_let_bound.t) ~body ->
    let env =
      match t.defining_expr, bound_vars with
      | Set_of_closures _, Set_of_closures { closure_vars; _ } ->
        Closure_id.Map.fold (fun _closure_id closure_var env ->
            let closure_var = VB.var closure_var in
            E.add_variable env closure_var K.value)
          closure_vars
          env
      | Set_of_closures _, Singleton _ ->
        Misc.fatal_errorf "Cannot bind a [Set_of_closures] to a \
            [Singleton]:@ %a"
          print t
      | _, Set_of_closures _ ->
        Misc.fatal_errorf "Cannot bind a non-[Set_of_closures] to a \
            [Set_of_closures]:@ %a"
          print t
      | Prim (prim, _dbg), Singleton var ->
        let var = VB.var var in
        E.add_variable env var (Flambda_primitive.result_kind' prim)
      | Simple simple, Singleton var ->
        let var = VB.var var in
        Simple.pattern_match simple
          ~const:(fun const -> E.add_variable env var (T.kind_for_const const))
          ~name:(fun name -> E.add_variable env var (E.kind_of_name env name))
    in
    Expr.invariant env body)

let defining_expr t = t.defining_expr

let free_names ({ bound_vars_and_body = _; defining_expr; } as t) =
  pattern_match t ~f:(fun ~bound_vars ~body ->
    let name_mode =
      Bindable_let_bound.name_mode bound_vars
    in
    let bound_vars = Bindable_let_bound.all_bound_vars' bound_vars in
    let from_defining_expr =
      Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
        (Named.free_names defining_expr)
        name_mode
    in
    let from_body = Expr.free_names body in
    (* CR mshinwell: See comment in expr.rec.ml *)
    Name_occurrences.union from_defining_expr
      (Name_occurrences.remove_vars from_body bound_vars))

let apply_name_permutation ({ bound_vars_and_body; defining_expr; } as t)
      perm =
  let bound_vars_and_body' =
    Bound_vars_and_body.apply_name_permutation bound_vars_and_body perm
  in
  let defining_expr' =
    Named.apply_name_permutation defining_expr perm
  in
  if bound_vars_and_body == bound_vars_and_body'
    && defining_expr == defining_expr'
  then t
  else
    { bound_vars_and_body = bound_vars_and_body';
      defining_expr = defining_expr';
    }

let all_ids_for_export { bound_vars_and_body; defining_expr; } =
  let defining_expr_ids =
    Named.all_ids_for_export defining_expr
  in
  let bound_vars_and_body_ids =
    Bound_vars_and_body.all_ids_for_export bound_vars_and_body
  in
  Ids_for_export.union defining_expr_ids bound_vars_and_body_ids

let import import_map { bound_vars_and_body; defining_expr; } =
  let defining_expr = Named.import import_map defining_expr in
  let bound_vars_and_body =
    Bound_vars_and_body.import import_map bound_vars_and_body
  in
  { bound_vars_and_body; defining_expr; }
