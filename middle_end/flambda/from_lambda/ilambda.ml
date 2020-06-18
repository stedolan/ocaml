(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2016--2019 OCamlPro SAS                                    *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type simple =
  | Var of Ident.t
  | Const of Lambda.structured_constant

type exn_continuation =
  { exn_handler : Continuation.t;
    extra_args : (simple * Lambda.value_kind) list;
  }

type trap_action =
  | Push of { exn_handler : Continuation.t; }
  | Pop of { exn_handler : Continuation.t; }

type user_visible =
  | User_visible
  | Not_user_visible

type t =
  | Let of Ident.t * user_visible * Lambda.value_kind * named * t
  | Let_mutable of let_mutable
  | Let_rec of function_declarations * t
  | Let_cont of let_cont
  | Apply of apply
  | Apply_cont of Continuation.t * trap_action option * simple list
  | Switch of Ident.t * switch

and named =
  | Simple of simple
  | Prim of {
      prim : Lambda.primitive;
      args : simple list;
      loc : Lambda.scoped_location;
      exn_continuation : exn_continuation option;
    }
  | Assign of { being_assigned : Ident.t; new_value : simple; }

and let_mutable = {
  id : Ident.t;
  initial_value : simple;
  contents_kind : Lambda.value_kind;
  body : t;
}

and function_declaration = {
  kind : Lambda.function_kind;
  return_continuation : Continuation.t;
  exn_continuation : exn_continuation;
  params : (Ident.t * Lambda.value_kind) list;
  return : Lambda.value_kind;
  body : t;
  free_idents_of_body : Ident.Set.t;
  (* [free_idents_of_body] saves writing a free variables function on
     Ilambda terms. *)
  attr : Lambda.function_attribute;
  loc : Lambda.scoped_location;
  stub : bool;
}

and function_declarations = (Ident.t * function_declaration) list

and let_cont = {
  name : Continuation.t;
  is_exn_handler : bool;
  params : (Ident.t * user_visible * Lambda.value_kind) list;
  recursive : Asttypes.rec_flag;
  body : t;
  handler : t;
}

and apply = {
  kind : apply_kind;
  func : Ident.t;
  args : simple list;
  continuation : Continuation.t;
  exn_continuation : exn_continuation;
  loc : Lambda.scoped_location;
  should_be_tailcall : bool;
  inlined : Lambda.inline_attribute;
  specialised : Lambda.specialise_attribute;
}

and apply_kind =
  | Function
  | Method of { kind : Lambda.meth_kind; obj : simple; }

and switch = {
  numconsts : int;
  consts : (int * Continuation.t * trap_action option * (simple list)) list;
  failaction : (Continuation.t * trap_action option * (simple list)) option;
}

type program = {
  expr : t;
  return_continuation : Continuation.t;
  exn_continuation : exn_continuation;
  uses_mutable_variables : bool;
}

let fprintf = Format.fprintf

let print_simple ppf simple =
  match simple with
  | Var id -> Ident.print ppf id
  | Const cst -> Printlambda.structured_constant ppf cst

let print_simple_and_value_kind ppf (simple, kind) =
  fprintf ppf "@[%a@ \u{2237}@ %a@]"
    print_simple simple
    Printlambda.value_kind' kind

let rec print_function ppf
      ({ return_continuation; kind; params; body; free_idents_of_body = _; attr;
         exn_continuation; return = _; loc = _; stub = _;
       } : function_declaration) =
  let pr_params ppf params =
    match kind with
    | Curried ->
      List.iter (fun (param, _) -> fprintf ppf "@ %a" Ident.print param)
        params
    | Tupled ->
      fprintf ppf " (";
      let first = ref true in
      List.iter (fun (param, _) ->
          if !first then first := false else fprintf ppf ",@ ";
          Ident.print ppf param)
        params;
      fprintf ppf ")"
  in
  fprintf ppf "@[<2>(function <%a> " Continuation.print return_continuation;
  begin match exn_continuation.extra_args with
  | [] -> fprintf ppf "<exn=%a>" Continuation.print exn_continuation.exn_handler
  | extra_args ->
    fprintf ppf "<exn=%a (%a)>"
      Continuation.print exn_continuation.exn_handler
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        print_simple_and_value_kind)
      extra_args
  end;
  fprintf ppf "%a@ %a%a)@]"
    pr_params params
    Printlambda.function_attribute attr
    print body

and print_named ppf (named : named) =
  match named with
  | Simple (Var id) -> Ident.print ppf id
  | Simple (Const cst) -> Printlambda.structured_constant ppf cst
  | Prim { prim; args; _ } ->
    fprintf ppf "@[<2>(%a %a)@]"
      Printlambda.primitive prim
      (Format.pp_print_list ~pp_sep:Format.pp_print_space print_simple) args
  | Assign { being_assigned; new_value; } ->
    fprintf ppf "@[<2>(assign@ %a@ %a)@]" Ident.print being_assigned
      print_simple new_value

and print_trap_action ppf trap_action =
  match trap_action with
  | None -> ()
  | Some (Push { exn_handler; }) ->
    fprintf ppf "push %a then "
      Continuation.print exn_handler
  | Some (Pop { exn_handler; }) ->
    fprintf ppf "pop %a then "
      Continuation.print exn_handler

and print ppf (t : t) =
  match t with
  | Apply ap ->
    let print_func_and_kind ppf func =
      match ap.kind with
      | Function -> Ident.print ppf func
      | Method { kind; obj; } ->
        Format.fprintf ppf "send%a %a#%a"
          Printlambda.meth_kind kind
          print_simple obj
          Ident.print func
    in
    fprintf ppf "@[<2>(apply@ %a<%a> %a%a%a%a)@]"
      print_func_and_kind ap.func
      Continuation.print ap.continuation
      (Format.pp_print_list ~pp_sep:Format.pp_print_space print_simple) ap.args
      Printlambda.apply_tailcall_attribute ap.should_be_tailcall
      Printlambda.apply_inlined_attribute ap.inlined
      Printlambda.apply_specialised_attribute ap.specialised
  | Let (id, _user_visible, kind, arg, body) ->
    let rec let_body = function
      | Let (id, _user_visible, kind, arg, body) ->
        fprintf ppf "@ @[<2>%a@ \u{2237}@ %a =@ %a@]"
          Ident.print id
          Printlambda.value_kind' kind
          print_named arg;
          let_body body
      | expr -> expr
    in
    fprintf ppf "@[<2>(let@ @[<v 1>(@[<2>%a@ \u{2237}@ %a =@ %a@]"
      Ident.print id
      Printlambda.value_kind' kind
      print_named arg;
    let expr = let_body body in
    fprintf ppf ")@]@ %a)@]" print expr
  | Let_mutable { id; initial_value; contents_kind; body; } ->
    fprintf ppf "@[<2>(let_mutable@ @[<v 1>(@[<2>%a =%a@ %a@]"
      Ident.print id
      Printlambda.value_kind' contents_kind
      print_simple initial_value;
    fprintf ppf ")@]@ %a)@]" print body
  | Let_rec (id_arg_list, body) ->
    let bindings ppf id_arg_list =
      let spc = ref false in
      List.iter (fun (id, l) ->
          if !spc then fprintf ppf "@ " else spc := true;
          fprintf ppf "@[<2>%a@ %a@]" Ident.print id print_function l)
        id_arg_list in
    fprintf ppf
      "@[<2>(let_rec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list print body
  | Switch (arg, sw) ->
    let switch ppf sw =
      let spc = ref false in
      List.iter (fun (n, cont, trap, args) ->
          if !spc then fprintf ppf "@ " else spc := true;
          match args with
          | [] ->
            fprintf ppf "@[<hv 1>| %i -> %agoto %a@]"
              n
              print_trap_action trap
              Continuation.print cont
          | args ->
            fprintf ppf "@[<hv 1>| %i -> %aapply_cont %a %a@]"
              n
              print_trap_action trap
              Continuation.print cont
              (Format.pp_print_list ~pp_sep:Format.pp_print_space print_simple)
              args)
        sw.consts;
      begin match sw.failaction with
      | None  -> ()
      | Some (l, trap, args) ->
        if !spc then fprintf ppf "@ " else spc := true;
        match args with
        | [] ->
          fprintf ppf "@[<hv 1>default:@ %agoto %a@]"
            print_trap_action trap
            Continuation.print l
        | args ->
          fprintf ppf "@[<hv 1>default:@ %aapply_cont %a %a@]"
            print_trap_action trap
            Continuation.print l
            (Format.pp_print_list ~pp_sep:Format.pp_print_space print_simple)
            args
      end in
    fprintf ppf
      "@[<1>(@[<v 1>%s %a@ @[<v 0>%a@]@])@]"
      (match sw.failaction with None -> "switch*" | _ -> "switch")
      Ident.print arg
      switch sw
  | Let_cont _ ->
    let rec gather_let_conts let_conts (t : t) =
      match t with
      | Let_cont let_cont ->
        gather_let_conts (let_cont :: let_conts) let_cont.body
      | body -> let_conts, body
    in
    let let_conts, body = gather_let_conts [] t in
    let print_let_cont ppf { name; params; recursive; handler;
          body = _; is_exn_handler; } =
      fprintf ppf "@[<v 2>where %a%s%s%(%)%a%(%) =@ %a@]"
        Continuation.print name
        (match recursive with Nonrecursive -> "" | Recursive -> "*")
        (if is_exn_handler then "<exn>" else "")
        ((match params with [] -> "" | _ -> " @[<h 2>(") : _ format6)
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          (fun ppf (ident, _user_visible, kind) ->
            Format.fprintf ppf "%a@ \u{2237}@ %a"
              Ident.print ident
              Printlambda.value_kind' kind)) params
        ((match params with [] -> "" | _ -> ")@]") : _ format6)
        print handler
    in
    let pp_sep ppf () = fprintf ppf "@ " in
    fprintf ppf "@[<2>(@[<v 0>%a@;@[<v 0>%a@]@])@]"
      print body
      (Format.pp_print_list ~pp_sep print_let_cont) let_conts
  | Apply_cont (i, trap_action, ls)  ->
    fprintf ppf "@[<2>(%aapply_cont@ %a@ %a)@]"
      print_trap_action trap_action
      Continuation.print i
      (Format.pp_print_list ~pp_sep:Format.pp_print_space print_simple) ls

let print_program ppf p =
  print ppf p.expr

let recursive_functions func_decls =
  let module SCC = Strongly_connected_components.Make (Ident) in
  let fun_ids = Ident.Set.of_list (List.map fst func_decls) in
  let directed_graph : Ident.Set.t Ident.Map.t =
    List.fold_left (fun graph (fun_id, decl) ->
        let free_fun_ids = Ident.Set.inter fun_ids decl.free_idents_of_body in
        Ident.Map.add fun_id free_fun_ids graph)
      Ident.Map.empty
      func_decls
  in
  let connected_components =
    SCC.connected_components_sorted_from_roots_to_leaf directed_graph
  in
  Array.fold_left (fun rec_ids component ->
      match component with
      | SCC.No_loop _ -> rec_ids
      | SCC.Has_loop elts -> List.fold_right Ident.Set.add elts rec_ids)
    Ident.Set.empty connected_components

let rec contains_closures t =
  match t with
  | Let (_, _, _, _, body)
  | Let_mutable { body; _ } -> contains_closures body
  | Let_rec _ -> true
  | Let_cont { body; handler; _ } ->
    contains_closures body || contains_closures handler
  | Apply _ | Apply_cont _ | Switch _ -> false
