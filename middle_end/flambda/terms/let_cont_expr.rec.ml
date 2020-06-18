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

type t =
  | Non_recursive of {
      handler : Non_recursive_let_cont_handler.t;
      num_free_occurrences : Name_occurrences.Num_occurrences.t;
    }
  | Recursive of Recursive_let_cont_handlers.t

(* CR mshinwell: A sketch of code for the invariant check is on cps_types. *)
let invariant _env _t = ()

let print_with_cache ~cache ppf t =
  if !Clflags.dump_let_cont then begin
    (* Printing the same way as for [Let] is easier when debugging lifting
       passes. *)
    Misc.fatal_error "Needs re-enabling"
(*
    let rec let_cont_body (expr : Expr.t) =
      match expr with
      | Let_cont { body; handlers; } ->
        fprintf ppf "@ @[<2>%a@]"
          (Let_cont_handlers.print_with_cache ~cache) handlers;
        let_cont_body body
      | _ -> ul
    in
    fprintf ppf "@[<2>(%slet_cont%s@ @[<hv 1>(@[<2>%a@]"
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
      (Let_cont_handlers.print_with_cache ~cache) handlers;
    let expr = let_cont_body body in
    fprintf ppf ")@]@ %a)@]" (print_with_cache0 ~cache) expr
*)
  end else begin
    let rec gather_let_conts let_conts let_cont =
      match let_cont with
      | Non_recursive { handler; num_free_occurrences = _; } ->
        Non_recursive_let_cont_handler.pattern_match handler
          ~f:(fun k ~(body : Expr.t) ->
            let let_conts, body =
              match Expr.descr body with
              | Let_cont let_cont -> gather_let_conts let_conts let_cont
              | _ -> let_conts, body
            in
            let handler = Non_recursive_let_cont_handler.handler handler in
            (k, Recursive.Non_recursive, handler) :: let_conts, body)
      | Recursive handlers ->
        Recursive_let_cont_handlers.pattern_match handlers
          ~f:(fun ~(body : Expr.t) handlers ->
            let handlers = Continuation_handlers.to_map handlers in
            let let_conts, body =
              match Expr.descr body with
              | Let_cont let_cont -> gather_let_conts let_conts let_cont
              | _ -> let_conts, body
            in
            let new_let_conts =
              List.map (fun (k, handler) -> k, Recursive.Recursive, handler)
                (Continuation.Map.bindings handlers)
            in
            new_let_conts @ let_conts, body)
    in
    let let_conts, body = gather_let_conts [] t in
    fprintf ppf "@[<v 1>(%a@;" (Expr.print_with_cache ~cache) body;
    let first = ref true in
    List.iter (fun (cont, recursive, handler) ->
        Continuation_handler.print_using_where_with_cache recursive ~cache
          ppf cont handler ~first:!first;
        first := false)
      (List.rev let_conts);
    fprintf ppf ")@]"
  end

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let create_non_recursive cont handler ~body =
  let free_names = Expr.free_names body in
  let num_free_occurrences =
    Name_occurrences.count_continuation free_names cont
  in
  (* We don't inline out linear uses of continuations here, as it could
     result in quadratic behaviour.  However we can safely avoid creating
     a completely unused continuation binding. *)
  match num_free_occurrences with
  | Zero -> body
  | One | More_than_one ->
    match Expr.descr body with
    | Apply_cont apply_cont when Apply_cont.is_goto_to apply_cont cont ->
      (* CR mshinwell: This could work for the >0 arity-case too, to handle
         continuation aliases. *)
      Continuation_params_and_handler.pattern_match
        (Continuation_handler.params_and_handler handler)
        ~f:(fun params ~handler:handler_expr ->
          match params with
          | [] -> handler_expr
          | _ ->
            Misc.fatal_errorf
              "Continuation handler expected to have zero arity: %a"
              Continuation_handler.print handler)
    | _ ->
      let handler = Non_recursive_let_cont_handler.create cont handler ~body in
      Expr.create_let_cont (Non_recursive { handler; num_free_occurrences; })

let create_recursive handlers ~body =
  if Continuation_handlers.contains_exn_handler handlers then begin
    Misc.fatal_error "Exception-handling continuations cannot be recursive"
  end;
  Expr.create_let_cont
    (Recursive (Recursive_let_cont_handlers.create handlers ~body))

let free_names t =
  match t with
  | Non_recursive { handler; num_free_occurrences = _; } ->
    Non_recursive_let_cont_handler.free_names handler
  | Recursive handlers ->
    Recursive_let_cont_handlers.free_names handlers

let apply_name_permutation t perm =
  match t with
  | Non_recursive { handler; num_free_occurrences; } ->
    let handler' =
      Non_recursive_let_cont_handler.apply_name_permutation handler perm
    in
    if handler == handler' then t
    else Non_recursive { handler = handler'; num_free_occurrences; }
  | Recursive handlers ->
    let handlers' =
      Recursive_let_cont_handlers.apply_name_permutation handlers perm
    in
    if handlers == handlers' then t
    else Recursive handlers'

let all_ids_for_export t =
  match t with
  | Non_recursive { handler; num_free_occurrences = _; } ->
    Non_recursive_let_cont_handler.all_ids_for_export handler
  | Recursive handlers ->
    Recursive_let_cont_handlers.all_ids_for_export handlers

let import import_map t =
  match t with
  | Non_recursive { handler; num_free_occurrences; } ->
    let handler =
      Non_recursive_let_cont_handler.import import_map handler
    in
    Non_recursive { handler; num_free_occurrences; }
  | Recursive handlers ->
    Recursive (Recursive_let_cont_handlers.import import_map handlers)

