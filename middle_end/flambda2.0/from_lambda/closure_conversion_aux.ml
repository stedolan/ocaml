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

module Env = struct
  type t = {
    variables : Variable.t Ident.Map.t;
    globals : Symbol.t Numbers.Int.Map.t;
    simples_to_substitute : Simple.t Ident.Map.t;
    still_at_toplevel : bool;
  }

  let empty = {
    variables = Ident.Map.empty;
    globals = Numbers.Int.Map.empty;
    simples_to_substitute = Ident.Map.empty;
    still_at_toplevel = true;
  }

  let clear_local_bindings env =
    { empty with
      globals = env.globals;
      still_at_toplevel = false;
    }

  let add_var t id var = { t with variables = Ident.Map.add id var t.variables }
  let add_vars t ids vars = List.fold_left2 add_var t ids vars
  let add_var_map t map =
    { t with variables = Ident.Map.union_right t.variables map }

  let add_var_like t id (user_visible : Ilambda.user_visible) =
    let user_visible =
      match user_visible with
      | Not_user_visible -> None
      | User_visible -> Some ()
    in
    let var = Variable.create_with_same_name_as_ident ?user_visible id in
    add_var t id var, var

  let add_vars_like t ids =
    let vars =
      List.map (fun (id, (user_visible : Ilambda.user_visible)) ->
          let user_visible =
            match user_visible with
            | Not_user_visible -> None
            | User_visible -> Some ()
          in
          Variable.create_with_same_name_as_ident ?user_visible id)
        ids
    in
    add_vars t (List.map fst ids) vars, vars

  (* CR mshinwell: Rethink the semantics of these re. fatal errors etc *)

  let find_var t id =
    try Ident.Map.find id t.variables
    with Not_found ->
      Misc.fatal_errorf "Closure_conversion.Env.find_var: %s@ %s"
        (Ident.unique_name id)
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack 42))

  let find_var_exn t id =
    Ident.Map.find id t.variables

  let find_name t id = Name.var (find_var t id)
  let find_name_exn t id = Name.var (find_var_exn t id)

  let find_vars t ids =
    List.map (fun id -> find_var t id) ids

  let add_global t pos symbol =
    { t with globals = Numbers.Int.Map.add pos symbol t.globals }

  let find_global t pos =
    try Numbers.Int.Map.find pos t.globals
    with Not_found ->
      Misc.fatal_error ("Closure_conversion.Env.find_global: global "
        ^ string_of_int pos)

  let add_simple_to_substitute t id simple =
    if Ident.Map.mem id t.simples_to_substitute then begin
      Misc.fatal_errorf "Cannot redefine [Simple] associated with %a"
        Ident.print id
    end;
    { t with
      simples_to_substitute = Ident.Map.add id simple t.simples_to_substitute;
    }

  let find_simple_to_substitute_exn t id =
    Ident.Map.find id t.simples_to_substitute

  let still_at_toplevel t = t.still_at_toplevel

  let no_longer_at_toplevel t =
    { t with
      still_at_toplevel = false;
    }
end

module Function_decls = struct
  module Function_decl = struct
    type t = {
      let_rec_ident : Ident.t;
      closure_id : Closure_id.t;
      kind : Lambda.function_kind;
      params : (Ident.t * Lambda.value_kind) list;
      return : Lambda.value_kind;
      return_continuation : Continuation.t;
      exn_continuation : Ilambda.exn_continuation;
      body : Ilambda.t;
      free_idents_of_body : Ident.Set.t;
      attr : Lambda.function_attribute;
      loc : Lambda.scoped_location;
      stub : bool;
      recursive : Recursive.t;
      contains_closures : bool;
    }

    let create ~let_rec_ident ~closure_id ~kind ~params ~return
        ~return_continuation ~exn_continuation ~body ~attr
        ~loc ~free_idents_of_body ~stub recursive =
      let let_rec_ident =
        match let_rec_ident with
        | None -> Ident.create_local "unnamed_function"
        | Some let_rec_ident -> let_rec_ident
      in
      let contains_closures = Ilambda.contains_closures body in
      { let_rec_ident;
        closure_id;
        kind;
        params;
        return;
        return_continuation;
        exn_continuation;
        body;
        free_idents_of_body;
        attr;
        loc;
        stub;
        recursive;
        contains_closures;
      }

    let let_rec_ident t = t.let_rec_ident
    let closure_id t = t.closure_id
    let kind t = t.kind
    let params t = t.params
    let return t = t.return
    let return_continuation t = t.return_continuation
    let exn_continuation t = t.exn_continuation
    let body t = t.body
    let free_idents t = t.free_idents_of_body
    let inline t = t.attr.inline
    let specialise t = t.attr.specialise
    let is_a_functor t = t.attr.is_a_functor
    let stub t = t.attr.stub
    let loc t = t.loc
    let recursive t = t.recursive
    let contains_closures t = t.contains_closures
  end

  type t = {
    function_decls : Function_decl.t list;
    all_free_idents : Ident.Set.t;
  }

  (* All identifiers free in the bodies of the given function declarations,
     indexed by the identifiers corresponding to the functions themselves. *)
  let free_idents_by_function function_decls =
    List.fold_right (fun decl map ->
        Closure_id.Map.add (Function_decl.closure_id decl)
          (Function_decl.free_idents decl) map)
      function_decls Closure_id.Map.empty

  let all_free_idents function_decls =
    Closure_id.Map.fold (fun _ -> Ident.Set.union)
      (free_idents_by_function function_decls) Ident.Set.empty

  (* All identifiers of simultaneously-defined functions in [ts]. *)
  let let_rec_idents function_decls =
    List.map Function_decl.let_rec_ident function_decls

  (* All parameters of functions in [ts]. *)
  let all_params function_decls =
    List.concat (List.map Function_decl.params function_decls)

  let set_diff (from : Ident.Set.t) (idents : Ident.t list) =
    List.fold_right Ident.Set.remove idents from

  (* CR-someday lwhite: use a different name from above or explain the
     difference *)
  let all_free_idents function_decls =
    set_diff (set_diff (all_free_idents function_decls)
        (List.map fst (all_params function_decls)))
      (let_rec_idents function_decls)

  let create function_decls =
    { function_decls;
      all_free_idents = all_free_idents function_decls;
    }

  let to_list t = t.function_decls

  let all_free_idents t = t.all_free_idents
end
