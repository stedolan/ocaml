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

module Bound_symbols = struct
  module Code_and_set_of_closures = struct
    type t = {
      code_ids : Code_id.Set.t;
      closure_symbols : Symbol.t Closure_id.Lmap.t
    }

    (* CR mshinwell: Share with [Bindable_let_bound] and below *)
    let print_closure_binding ppf (closure_id, sym) =
      Format.fprintf ppf "@[%a @<0>%s\u{21a4}@<0>%s %a@]"
        Symbol.print sym
        (Flambda_colours.elide ())
        (Flambda_colours.elide ())
        Closure_id.print closure_id

    let print ppf { code_ids; closure_symbols; } =
      match
        Code_id.Set.elements code_ids,
        Closure_id.Lmap.bindings closure_symbols
      with
      | [code_id], [] ->
        Format.fprintf ppf "%a" Code_id.print code_id
      | [], [closure_binding] ->
        Format.fprintf ppf "@<0>%s%a@<0>%s"
          (Flambda_colours.symbol ())
          print_closure_binding closure_binding
          (Flambda_colours.normal ())
      | _, _ ->
        Format.fprintf ppf "@[<hov 1>\
            @[<hov 1>(code_ids@ (%a))@]@ \
            @[<hov 1>(closure_symbols@ {%a})@]\
            @]"
          Code_id.Set.print code_ids
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
            print_closure_binding)
          (Closure_id.Lmap.bindings closure_symbols)

    let being_defined { code_ids = _; closure_symbols; } =
      Closure_id.Lmap.fold (fun _closure_id symbol being_defined ->
          Symbol.Set.add symbol being_defined)
        closure_symbols
        Symbol.Set.empty

    let closure_symbols_being_defined t = being_defined t

    let code_being_defined { code_ids; closure_symbols = _; } = code_ids

    let free_names { code_ids; closure_symbols; } =
      let from_code_ids =
        Code_id.Set.fold (fun code_id from_code_ids ->
            Name_occurrences.add_code_id from_code_ids code_id Name_mode.normal)
          code_ids
          Name_occurrences.empty
      in
      Closure_id.Lmap.fold (fun _closure_id closure_sym bound_names ->
          Name_occurrences.add_symbol bound_names closure_sym Name_mode.normal)
        closure_symbols
        from_code_ids

    let all_ids_for_export { code_ids; closure_symbols; } =
      let symbols =
        Closure_id.Lmap.fold (fun _closure_id sym symbols ->
            Symbol.Set.add sym symbols)
          closure_symbols
          Symbol.Set.empty
      in
      Ids_for_export.create ~code_ids ~symbols ()

    let import import_map { code_ids; closure_symbols; } =
      let code_ids =
        Code_id.Set.map (Ids_for_export.Import_map.code_id import_map) code_ids
      in
      let closure_symbols =
        Closure_id.Lmap.map (Ids_for_export.Import_map.symbol import_map)
          closure_symbols
      in
      { code_ids; closure_symbols; }
  end

  type t =
    | Singleton of Symbol.t
    | Sets_of_closures of Code_and_set_of_closures.t list

  let print ppf t =
    match t with
    | Singleton sym ->
      Format.fprintf ppf "@[%a@ \u{2237}@ %a@]"
        Symbol.print sym
        K.print K.value
    | Sets_of_closures [set] ->
      Code_and_set_of_closures.print ppf set
    | Sets_of_closures sets ->
      Format.fprintf ppf "@[<hov 1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          Code_and_set_of_closures.print)
        sets

  let print_with_cache ~cache:_ ppf t = print ppf t

  (* CR mshinwell: This should have an [invariant] function.  One thing to
     check is that the [closure_symbols] are all distinct. *)

  let invariant _ t =
    match t with
    | Singleton _ -> ()
    | Sets_of_closures sets ->
      List.iter (fun { Code_and_set_of_closures.closure_symbols; _ } ->
        Closure_id.Lmap.invariant closure_symbols
      ) sets

  let being_defined t =
    match t with
    | Singleton sym -> Symbol.Set.singleton sym
    | Sets_of_closures sets ->
      List.fold_left (fun being_defined set ->
          Symbol.Set.union (Code_and_set_of_closures.being_defined set)
            being_defined)
        Symbol.Set.empty
        sets

  let closure_symbols_being_defined t =
    match t with
    | Singleton _sym -> Symbol.Set.empty
    | Sets_of_closures sets ->
      Symbol.Set.union_list
        (List.map Code_and_set_of_closures.closure_symbols_being_defined sets)

  let code_being_defined t =
    match t with
    | Singleton _ -> Code_id.Set.empty
    | Sets_of_closures sets ->
      Code_id.Set.union_list
        (List.map Code_and_set_of_closures.code_being_defined sets)

  let everything_being_defined t =
    let code =
      Code_id.Set.fold (fun code_id code ->
          Code_id_or_symbol.Set.add (Code_id code_id) code)
        (code_being_defined t)
        Code_id_or_symbol.Set.empty
    in
    let closure_symbols =
      Symbol.Set.fold (fun symbol closure_symbols ->
          Code_id_or_symbol.Set.add (Symbol symbol) closure_symbols)
        (being_defined t)
        Code_id_or_symbol.Set.empty
    in
    Code_id_or_symbol.Set.union code closure_symbols

  let apply_name_permutation t _perm = t

  let free_names t =
    match t with
    | Singleton sym -> Name_occurrences.singleton_symbol sym Name_mode.normal
    | Sets_of_closures sets ->
      Name_occurrences.union_list
        (List.map Code_and_set_of_closures.free_names sets)

  let all_ids_for_export t =
    match t with
    | Singleton sym -> Ids_for_export.add_symbol (Ids_for_export.empty) sym
    | Sets_of_closures sets ->
      List.fold_left (fun ids set ->
          Ids_for_export.union ids
            (Code_and_set_of_closures.all_ids_for_export set))
        Ids_for_export.empty
        sets

  let import import_map t =
    match t with
    | Singleton sym ->
      Singleton (Ids_for_export.Import_map.symbol import_map sym)
    | Sets_of_closures sets ->
      let sets = List.map (Code_and_set_of_closures.import import_map) sets in
      Sets_of_closures sets
end

module Scoping_rule = struct
  type t =
    | Syntactic
    | Dominator

  let print ppf t =
    match t with
    | Syntactic -> Format.fprintf ppf "Syntactic"
    | Dominator -> Format.fprintf ppf "Dominator"
end

type t = {
  scoping_rule : Scoping_rule.t;
  bound_symbols : Bound_symbols.t;
  defining_expr : Static_const.t;
  body : Expr.t;
}

let create scoping_rule bound_symbols defining_expr body =
  { scoping_rule;
    bound_symbols;
    defining_expr;
    body;
  }

let scoping_rule t = t.scoping_rule
let bound_symbols t = t.bound_symbols
let defining_expr t = t.defining_expr
let body t = t.body

type flattened_for_printing_descr =
  | Code of Code_id.t * Static_const.Code.t
  | Set_of_closures of Symbol.t Closure_id.Lmap.t * Set_of_closures.t
  | Other of Symbol.t * Static_const.t

type flattened_for_printing = {
  second_or_later_binding_within_one_set : bool;
  second_or_later_set_of_closures : bool;
  descr : flattened_for_printing_descr;
  scoping_rule : Scoping_rule.t;
}

let shape_colour descr =
  match descr with
  | Code _ -> Flambda_colours.code_id ()
  | Set_of_closures _ | Other _ -> Flambda_colours.symbol ()

let flatten_for_printing { scoping_rule; bound_symbols; defining_expr; _ } =
  match bound_symbols with
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

let flatten t : _ * Expr.t =
  let rec flatten (expr : Expr.t) : _ * Expr.t =
    match Expr.descr expr with
    | Let_symbol t ->
      let flattened = flatten_for_printing t in
      let flattened', body = flatten t.body in
      flattened @ flattened', body
    | _ -> [], expr
  in
  let flattened = flatten_for_printing t in
  let flattened', body = flatten t.body in
  flattened @ flattened', body

let print_with_cache ~cache ppf t =
  let rec print_more flattened =
    match flattened with
    | [] -> ()
    | flat::flattened ->
      fprintf ppf "@ ";
      print_flattened ppf flat;
      print_more flattened
  in
  let flattened, body = flatten t in
  match flattened with
  | [] -> assert false
  | flat::flattened ->
    fprintf ppf "@[<v 1>(@<0>%slet_symbol@<0>%s@ @[<v 0>%a"
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
      print_flattened flat;
    print_more flattened;
    fprintf ppf "@]@ %a)@]" (Expr.print_with_cache ~cache) body

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant env
      { scoping_rule = _; bound_symbols; defining_expr = _; body; } =
  (* Static_const.invariant env defining_expr; *) (* CR mshinwell: FIXME *)
  Bound_symbols.invariant env bound_symbols;
  Expr.invariant env body

let free_names { scoping_rule = _; bound_symbols; defining_expr; body; } =
  let from_bound_symbols = Bound_symbols.free_names bound_symbols in
  let from_defining_expr =
    match bound_symbols with
    | Singleton _ -> Static_const.free_names defining_expr
    | Sets_of_closures _ ->
      Name_occurrences.diff (Static_const.free_names defining_expr)
        from_bound_symbols
  in
  Name_occurrences.union from_defining_expr
    (Name_occurrences.diff (Expr.free_names body) from_bound_symbols)

let apply_name_permutation
      ({ scoping_rule; bound_symbols; defining_expr; body; } as t) perm =
  let defining_expr' = Static_const.apply_name_permutation defining_expr perm in
  let body' = Expr.apply_name_permutation body perm in
  if defining_expr == defining_expr' && body == body' then t
  else
    { scoping_rule;
      bound_symbols;
      defining_expr = defining_expr';
      body = body';
    }

let all_ids_for_export
      { scoping_rule = _; bound_symbols; defining_expr; body; } =
  let bound_symbols_ids = Bound_symbols.all_ids_for_export bound_symbols in
  let defining_expr_ids = Static_const.all_ids_for_export defining_expr in
  let body_ids = Expr.all_ids_for_export body in
  Ids_for_export.union bound_symbols_ids
    (Ids_for_export.union defining_expr_ids body_ids)

let import import_map { scoping_rule; bound_symbols; defining_expr; body; } =
  let bound_symbols = Bound_symbols.import import_map bound_symbols in
  let defining_expr = Static_const.import import_map defining_expr in
  let body = Expr.import import_map body in
  { scoping_rule; bound_symbols; defining_expr; body; }

(* CR mshinwell: Add a type to just encapsulate bound_symbols and
   defining_expr. *)
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
