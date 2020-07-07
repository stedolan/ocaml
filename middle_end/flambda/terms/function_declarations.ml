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
  funs : Function_declaration.t Closure_id.Map.t;
  in_order : Function_declaration.t Closure_id.Lmap.t
}

let invariant _env _t = ()

let empty =
  { funs = Closure_id.Map.empty;
    in_order = Closure_id.Lmap.empty
  }

let is_empty { funs; _ } =
  Closure_id.Map.is_empty funs

let create in_order =
  { funs = Closure_id.Map.of_list (Closure_id.Lmap.bindings in_order);
    in_order
  }

let funs t = t.funs

let funs_in_order t = t.in_order

let find ({ funs; _ } : t) closure_id =
  Closure_id.Map.find closure_id funs

let print_with_cache ~cache ppf { in_order; _ } =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Closure_id.Lmap.print (Function_declaration.print_with_cache ~cache))
    in_order

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let free_names { funs; _ } =
  Closure_id.Map.fold
    (fun _closure_id (func_decl : Function_declaration.t) syms ->
      Name_occurrences.union syms (Function_declaration.free_names func_decl))
    funs
    (Name_occurrences.empty)

let apply_name_permutation ({ in_order; _ } as t) perm =
  let in_order' =
    Closure_id.Lmap.map_sharing (fun func_decl ->
        Function_declaration.apply_name_permutation func_decl perm)
      in_order
  in
  if in_order == in_order' then t
  else create in_order'

let all_ids_for_export { funs; _ } =
  Closure_id.Map.fold
    (fun _closure_id (func_decl : Function_declaration.t) ids ->
      Ids_for_export.union ids
        (Function_declaration.all_ids_for_export func_decl))
    funs
    Ids_for_export.empty

let import import_map { in_order; _ } =
  let in_order =
    Closure_id.Lmap.map (Function_declaration.import import_map) in_order
  in
  create in_order

let compare { funs = funs1; _ } { funs = funs2; _ } =
  Closure_id.Map.compare Function_declaration.compare funs1 funs2

let filter t ~f =
  let funs = Closure_id.Map.filter f t.funs in
  let in_order = Closure_id.Lmap.filter f t.in_order in
  { funs; in_order; }
