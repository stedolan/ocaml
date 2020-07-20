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

module T = Flambda_type
module VB = Var_in_binding_pos

module A = Name_abstraction.Make (Bindable_let_bound) (Expr)

type t = {
  name_abstraction : A.t;
  defining_expr : Named.t;
}

let pattern_match t ~f =
  A.pattern_match t.name_abstraction
    ~f:(fun bindable_let_bound body -> f bindable_let_bound ~body)

let pattern_match_pair t1 t2 ~f =
  A.pattern_match_pair t1.name_abstraction t2.name_abstraction
    ~f:(fun bindable_let_bound body1 body2 ->
      f bindable_let_bound ~body1 ~body2)

(* For printing "let symbol": *)

type flattened_for_printing_descr =
  | Code of Code_id.t * Static_const.Code.t
  | Set_of_closures of Symbol.t Closure_id.Lmap.t * Set_of_closures.t
  | Other of Symbol.t * Static_const.t

type flattened_for_printing = {
  second_or_later_binding_within_one_set : bool;
  second_or_later_set_of_closures : bool;
  descr : flattened_for_printing_descr;
  scoping_rule : Symbol_scoping_rule.t;
}

let shape_colour descr =
  match descr with
  | Code _ -> Flambda_colours.code_id ()
  | Set_of_closures _ | Other _ -> Flambda_colours.symbol ()

let flatten_for_printing0 bound_symbols scoping_rule defining_expr =
  match (bound_symbols : Bound_symbols.t) with
  | Singleton symbol ->
    [{ second_or_later_binding_within_one_set = false;
       second_or_later_set_of_closures = false;
       descr = Other (symbol, defining_expr);
       scoping_rule;
    }]
  | Sets_of_closures bound_symbol_components ->
    let code_and_sets_of_closures =
      Static_const.must_be_sets_of_closures defining_expr
    in
    let flattened, _ =
      List.fold_left2
        (fun (flattened_acc, second_or_later_set_of_closures)
             ({ code_ids = _; closure_symbols; }
                : Bound_symbols.Code_and_set_of_closures.t)
             ({ code; set_of_closures; }
                : Static_const.Code_and_set_of_closures.t) ->
          let flattened, _ =
            Code_id.Lmap.fold (fun code_id code (flattened', first) ->
                let flattened =
                  { second_or_later_binding_within_one_set = not first;
                    second_or_later_set_of_closures;
                    descr = Code (code_id, code);
                    scoping_rule;
                  }
                in
                flattened :: flattened', false)
              code
              ([], true)
          in
          let flattened' =
            if Set_of_closures.is_empty set_of_closures then []
            else
              let second_or_later_binding_within_one_set =
                not (Code_id.Lmap.is_empty code)
              in
              [{ second_or_later_binding_within_one_set;
                 second_or_later_set_of_closures;
                 descr = Set_of_closures (closure_symbols, set_of_closures);
                 scoping_rule;
              }]
          in
          let flattened_acc =
            flattened_acc @ (List.rev flattened) @ (List.rev flattened')
          in
          flattened_acc, true)
        ([], false)
        bound_symbol_components
        code_and_sets_of_closures
    in
    flattened

let flatten_for_printing t =
  pattern_match t ~f:(fun (bindable_let_bound : Bindable_let_bound.t) ~body ->
    match bindable_let_bound with
    | Symbols { bound_symbols; scoping_rule; } ->
      let flattened =
        flatten_for_printing0 bound_symbols scoping_rule
          (Named.must_be_static_const t.defining_expr)
      in
      Some (flattened, body)
    | Singleton _ | Set_of_closures _ -> None)

let print_closure_binding ppf (closure_id, sym) =
  Format.fprintf ppf "@[%a @<0>%s\u{21a4}@<0>%s %a@]"
    Symbol.print sym
    (Flambda_colours.elide ())
    (Flambda_colours.elide ())
    Closure_id.print closure_id

let print_flattened_descr_lhs ppf descr =
  match descr with
  | Code (code_id, _) -> Code_id.print ppf code_id
  | Set_of_closures (closure_symbols, _) ->
    Format.fprintf ppf "@[<hov 0>%a@]"
      (Format.pp_print_list
        ~pp_sep:(fun ppf () ->
          Format.fprintf ppf "@<0>%s,@ @<0>%s"
            (Flambda_colours.elide ())
            (Flambda_colours.normal ()))
        print_closure_binding)
      (Closure_id.Lmap.bindings closure_symbols)
  | Other (symbol, _) -> Symbol.print ppf symbol

(* CR mshinwell: Use [print_with_cache]? *)
let print_flattened_descr_rhs ppf descr =
  match descr with
  | Code (_, code) -> Static_const.Code.print ppf code
  | Set_of_closures (_, set) -> Set_of_closures.print ppf set
  | Other (_, static_const) -> Static_const.print ppf static_const

let print_flattened ppf
      { second_or_later_binding_within_one_set;
        second_or_later_set_of_closures;
        descr;
        scoping_rule;
      } =
  fprintf ppf "@[<hov 1>";
  if second_or_later_set_of_closures
    && not second_or_later_binding_within_one_set
  then begin
    fprintf ppf "@<0>%sand_set @<0>%s"
      (Flambda_colours.elide ())
      (Flambda_colours.normal ())
  end else if second_or_later_binding_within_one_set then begin
    fprintf ppf "@<0>%sand @<0>%s"
      (Flambda_colours.elide ())
      (Flambda_colours.normal ())
  end else begin
    let shape =
      match scoping_rule with
      | Syntactic -> "\u{25b6}" (* filled triangle *)
      | Dominator -> "\u{25b7}" (* unfilled triangle *)
    in
    fprintf ppf "@<0>%s@<1>%s @<0>%s"
      (shape_colour descr)
      shape
      (Flambda_colours.normal ())
  end;
  fprintf ppf
    "%a@<0>%s =@<0>%s@ %a@]"
    print_flattened_descr_lhs descr
    (Flambda_colours.elide ())
    (Flambda_colours.normal ())
    print_flattened_descr_rhs descr

let flatten_let_symbol t : _ * Expr.t =
  let rec flatten (expr : Expr.t) : _ * Expr.t =
    match Expr.descr expr with
    | Let t ->
      begin match flatten_for_printing t with
      | Some (flattened, body) ->
        let flattened', body = flatten body in
        flattened @ flattened', body
      | None -> [], expr
      end
    | _ -> [], expr
  in
  match flatten_for_printing t with
  | Some (flattened, body) ->
    let flattened', body = flatten body in
    flattened @ flattened', body
  | None -> assert false  (* see below *)

(* CR mshinwell: Merge the "let symbol" and "normal let" cases to use the
   same flattened type? *)
let print_let_symbol_with_cache ~cache ppf t =
  let rec print_more flattened =
    match flattened with
    | [] -> ()
    | flat::flattened ->
      fprintf ppf "@ ";
      print_flattened ppf flat;
      print_more flattened
  in
  let flattened, body = flatten_let_symbol t in
  match flattened with
  | [] -> assert false
  | flat::flattened ->
    fprintf ppf "@[<v 1>(@<0>%slet_symbol@<0>%s@ @[<v 0>%a"
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
      print_flattened flat;
    print_more flattened;
    fprintf ppf "@]@ %a)@]" (Expr.print_with_cache ~cache) body

(* For printing all kinds of let-expressions: *)

let print_with_cache ~cache ppf
      ({ name_abstraction = _; defining_expr; } as t) =
  let let_bound_var_colour bindable_let_bound =
    let kind = Bindable_let_bound.name_mode bindable_let_bound in
    if Name_mode.is_phantom kind then Flambda_colours.elide ()
    else Flambda_colours.variable ()
  in
  let rec let_body (expr : Expr.t) =
    match Expr.descr expr with
    | Let ({ name_abstraction = _; defining_expr; } as t) ->
      pattern_match t
        ~f:(fun (bindable_let_bound : Bindable_let_bound.t) ~body ->
          match bindable_let_bound with
          | Singleton _ | Set_of_closures _ ->
            fprintf ppf
              "@ @[<hov 1>@<0>%s%a@<0>%s =@<0>%s@ %a@]"
              (let_bound_var_colour bindable_let_bound)
              Bindable_let_bound.print bindable_let_bound
              (Flambda_colours.elide ())
              (Flambda_colours.normal ())
              (Named.print_with_cache ~cache) defining_expr;
            let_body body
          | Symbols _ -> expr)
    | _ -> expr
  in
  pattern_match t ~f:(fun (bindable_let_bound : Bindable_let_bound.t) ~body ->
    match bindable_let_bound with
    | Symbols _ -> print_let_symbol_with_cache ~cache ppf t
    | Singleton _ | Set_of_closures _ ->
      fprintf ppf "@[<v 1>(@<0>%slet@<0>%s@ (@[<v 0>\
          @[<hov 1>@<0>%s%a@<0>%s =@<0>%s@ %a@]"
        (Flambda_colours.expr_keyword ())
        (Flambda_colours.normal ())
        (let_bound_var_colour bindable_let_bound)
        Bindable_let_bound.print bindable_let_bound
        (Flambda_colours.elide ())
        (Flambda_colours.normal ())
        (Named.print_with_cache ~cache) defining_expr;
      let expr = let_body body in
      fprintf ppf "@])@ %a)@]"
        (Expr.print_with_cache ~cache) expr)

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let create bindable_let_bound ~defining_expr ~body =
  { name_abstraction = A.create bindable_let_bound body;
    defining_expr;
  }

let invariant env t =
  let module E = Invariant_env in
  Named.invariant env t.defining_expr;
  pattern_match t ~f:(fun (bindable_let_bound : Bindable_let_bound.t) ~body ->
    let env =
      match t.defining_expr, bindable_let_bound with
      | Set_of_closures _, Set_of_closures { closure_vars; _ } ->
        List.fold_left (fun env closure_var ->
            let closure_var = VB.var closure_var in
            E.add_variable env closure_var K.value)
          env
          closure_vars
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
      | Static_const _, Symbols _ -> env
      | Static_const _, Singleton _ ->
        Misc.fatal_errorf "Cannot bind a [Static_const] to a [Singleton]:@ %a"
          print t
      | (Simple _ | Prim _ | Set_of_closures _), Symbols _ ->
        Misc.fatal_errorf "Cannot bind a non-[Static_const] to [Symbols]:@ %a"
          print t
    in
    Expr.invariant env body)

let defining_expr t = t.defining_expr

let free_names ({ name_abstraction = _; defining_expr; } as t) =
  pattern_match t ~f:(fun bindable_let_bound ~body ->
    let from_bindable = Bindable_let_bound.free_names bindable_let_bound in
    let from_defining_expr =
      let name_mode = Bindable_let_bound.name_mode bindable_let_bound in
      Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
        (Named.free_names defining_expr)
        name_mode
    in
    let from_body = Expr.free_names body in
    (* CR mshinwell: See comment in expr.rec.ml *)
    Name_occurrences.union from_defining_expr
      (Name_occurrences.diff from_body from_bindable))

let apply_name_permutation ({ name_abstraction; defining_expr; } as t) perm =
  let name_abstraction' = A.apply_name_permutation name_abstraction perm in
  let defining_expr' = Named.apply_name_permutation defining_expr perm in
  if name_abstraction == name_abstraction' && defining_expr == defining_expr'
  then t
  else
    { name_abstraction = name_abstraction';
      defining_expr = defining_expr';
    }

let all_ids_for_export { name_abstraction; defining_expr; } =
  let defining_expr_ids = Named.all_ids_for_export defining_expr in
  let name_abstraction_ids = A.all_ids_for_export name_abstraction in
  Ids_for_export.union defining_expr_ids name_abstraction_ids

let import import_map { name_abstraction; defining_expr; } =
  let defining_expr = Named.import import_map defining_expr in
  let name_abstraction = A.import import_map name_abstraction in
  { name_abstraction; defining_expr; }
