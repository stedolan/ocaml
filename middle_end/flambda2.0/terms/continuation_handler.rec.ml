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

type t = {
  params_and_handler : Continuation_params_and_handler.t;
  stub : bool;
  is_exn_handler : bool;
}

let invariant _env _t = ()

let print_using_where_with_cache (recursive : Recursive.t) ~cache ppf k
      ({ params_and_handler = _; stub; is_exn_handler; } as t) ~first =
  if not first then begin
    fprintf ppf "@ "
  end;
  Continuation_params_and_handler.pattern_match t.params_and_handler
    ~f:(fun params ~handler ->
      begin match Expr.descr handler with
      | Apply_cont _ | Invalid _ -> fprintf ppf "@[<hov 1>"
      | _ -> fprintf ppf "@[<v 1>"
      end;
      fprintf ppf "@<0>%s%a@<0>%s@<0>%s%s@<0>%s%s@<0>%s%s@<0>%s"
        (Flambda_colours.continuation_definition ())
        Continuation.print k
        (Flambda_colours.normal ())
        (Flambda_colours.expr_keyword ())
        (match recursive with Non_recursive -> "" | Recursive -> " (rec)")
        (Flambda_colours.normal ())
        (if stub then " *stub*" else "")
        (Flambda_colours.continuation_annotation ())
        (if is_exn_handler then "[eh]" else "")
        (Flambda_colours.normal ());
      if List.length params > 0 then begin
        fprintf ppf " %a" Kinded_parameter.List.print params
      end;
      fprintf ppf "@<0>%s:@<0>%s@ %a"
        (Flambda_colours.elide ())
        (Flambda_colours.normal ())
        (Expr.print_with_cache ~cache) handler;
      fprintf ppf "@]")

let print_with_cache ~cache ppf { params_and_handler; stub; is_exn_handler; } =
  Format.fprintf ppf "@[<hov 1>\
      @[<hov 1>(params_and_handler@ %a)@]@ \
      @[<hov 1>(stub@ %b)@]@ \
      @[<hov 1>(is_exn_handler@ %b)@]\
      @]"
    (Continuation_params_and_handler.print_with_cache ~cache) params_and_handler
    stub
    is_exn_handler

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

(*
let print_with_cache ~cache ppf (t : t) =
  match t with
  | Non_recursive { name; handler = {
      params; stub; handler; is_exn_handler; }; } ->
    fprintf ppf "%a@ %s%s%a=@ %a"
      Continuation.print name
      (if stub then "*stub* " else "")
      (if is_exn_handler then "*exn* " else "")
      (Flambda_type.Parameters.print_or_omit_with_cache ~cache) params
      (Expr.print_with_cache ~cache) handler
  | Recursive handlers ->
    let first = ref true in
    Continuation.Map.iter (fun name
            { Continuation_handler.params; stub; is_exn_handler; handler; } ->
        if !first then begin
          fprintf ppf "@;rec "
        end else begin
          fprintf ppf "@;and "
        end;
        fprintf ppf "%a@ %s%s%a=@ %a"
          Continuation.print name
          (if stub then "*stub* " else "")
          (if is_exn_handler then "*exn* " else "")
          (Flambda_type.Parameters.print_or_omit_with_cache ~cache) params
          (Expr.print_with_cache ~cache) handler;
        first := false)
      handlers

let print_with_cache ~cache ppf { params_and_handler; stub; handler; } =
  Continuation_params_and_handler.pattern_match params_and_handler
    ~f:(fun params ~handler ->
      fprintf ppf "%s%s%a@ =@ %a"
        (if stub then "*stub* " else "")
        (if is_exn_handler then "*exn* " else "")
        (Flambda_type.Parameters.print_or_omit_with_cache ~cache) params
        Expr.print handler)

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t
*)

let create ~params_and_handler ~stub ~is_exn_handler =
  { params_and_handler;
    stub;
    is_exn_handler;
  }

let params_and_handler t = t.params_and_handler
let stub t = t.stub
let is_exn_handler t = t.is_exn_handler

let free_names
      { params_and_handler; stub = _; is_exn_handler = _; } =
  Continuation_params_and_handler.free_names params_and_handler

let apply_name_permutation
      ({ params_and_handler; stub;
         is_exn_handler; } as t) perm =
  let params_and_handler' =
    Continuation_params_and_handler.apply_name_permutation
      params_and_handler perm
  in
  if params_and_handler == params_and_handler' then t
  else
    { params_and_handler = params_and_handler';
      stub;
      is_exn_handler;
    }

let all_ids_for_export
      { params_and_handler; stub = _; is_exn_handler = _; } =
  Continuation_params_and_handler.all_ids_for_export params_and_handler

let import import_map
      { params_and_handler; stub; is_exn_handler; } =
  let params_and_handler =
    Continuation_params_and_handler.import import_map params_and_handler
  in
  { params_and_handler; stub; is_exn_handler; }

type behaviour =
  | Unreachable of { arity : Flambda_arity.t; }
  | Alias_for of { arity : Flambda_arity.t; alias_for : Continuation.t; }
  | Unknown of { arity : Flambda_arity.t; }

let behaviour t : behaviour =
  (* CR mshinwell: Maybe [behaviour] should be cached, to avoid re-opening
     the abstraction? *)
  (* This could be replaced by a more sophisticated analysis, but for the
     moment we just use a simple syntactic check. *)
  Continuation_params_and_handler.pattern_match t.params_and_handler
    ~f:(fun params ~handler ->
      let arity = Kinded_parameter.List.arity params in
      if t.is_exn_handler then
        Unknown { arity; }
      else
        match Expr.descr handler with
        | Apply_cont apply_cont ->
          begin match Apply_cont.trap_action apply_cont with
          | Some _ -> Unknown { arity; }
          | None ->
            let args = Apply_cont.args apply_cont in
            let params = List.map KP.simple params in
            if Misc.Stdlib.List.compare Simple.compare args params = 0 then
              Alias_for {
                arity;
                alias_for = Apply_cont.continuation apply_cont;
              }
            else
              Unknown { arity; }
          end
        | Invalid Treat_as_unreachable -> Unreachable { arity; }
        | _ -> Unknown { arity; })

let arity t =
  Continuation_params_and_handler.pattern_match t.params_and_handler
    ~f:(fun params ~handler:_ -> Kinded_parameter.List.arity params)

let with_params_and_handler t params_and_handler =
  { t with params_and_handler; }
