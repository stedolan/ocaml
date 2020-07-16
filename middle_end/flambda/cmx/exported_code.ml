(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module P = Flambda.Function_params_and_body

module Calling_convention = struct
  type t = {
    needs_closure_arg : bool;
  }

  let needs_closure_arg t = t.needs_closure_arg

  let print ppf { needs_closure_arg; } =
    Format.fprintf ppf
      "@[<hov 1>(needs_closure_arg@ %b)@]"
      needs_closure_arg

  let equal
        { needs_closure_arg = needs_closure_arg1; }
        { needs_closure_arg = needs_closure_arg2; } =
    Bool.equal needs_closure_arg1 needs_closure_arg2

  let compute ~params_and_body =
    let f ~return_continuation:_ _exn_continuation _params ~body ~my_closure =
      let free_vars = Flambda.Expr.free_names body in
      let needs_closure_arg = Name_occurrences.mem_var free_vars my_closure in
      { needs_closure_arg; }
    in
    P.pattern_match params_and_body ~f
end

type t0 =
  | Present of {
    params_and_body : P.t;
    calling_convention : Calling_convention.t;
  }
  | Imported of { calling_convention : Calling_convention.t; }

type t = t0 Code_id.Map.t

let print0 ppf t0 =
  match t0 with
  | Present { params_and_body; calling_convention; } ->
    Format.fprintf ppf
      "@[<hov 1>(Present@ (\
         @[<hov 1>(params_and_body@ %a)@]\
         @[<hov 1>(calling_convention@ %a)@]\
       ))@]"
      P.print params_and_body
      Calling_convention.print calling_convention
  | Imported { calling_convention; } ->
    Format.fprintf ppf
      "@[<hov 1>(Imported@ (calling_convention@ %a))@]"
      Calling_convention.print calling_convention

let print ppf t =
  Code_id.Map.print print0 ppf t

let empty = Code_id.Map.empty

let add_code code t =
  let with_calling_convention =
    Code_id.Map.map (fun params_and_body ->
        let calling_convention =
          Calling_convention.compute ~params_and_body
        in
        Present { params_and_body; calling_convention; })
      code
  in
  Code_id.Map.disjoint_union with_calling_convention t

let mark_as_imported t =
  let forget_params_and_body t0 =
    match t0 with
    | Imported _ -> t0
    | Present { params_and_body = _; calling_convention; } ->
      Imported { calling_convention; }
  in
  Code_id.Map.map forget_params_and_body t

let merge t1 t2 =
  let merge_one code_id t01 t02 =
    match t01, t02 with
    | Imported { calling_convention = cc1; },
      Imported { calling_convention = cc2; } ->
      if Calling_convention.equal cc1 cc2 then Some t01
      else
        Misc.fatal_errorf
          "Code id %a is imported with different calling conventions\
           (%a and %a)"
          Code_id.print code_id
          Calling_convention.print cc1
          Calling_convention.print cc2
    | Present _, Present _ ->
      Misc.fatal_errorf "Cannot merge two definitions for code id %a"
        Code_id.print code_id
    | Imported { calling_convention = cc_imported; },
      (Present { calling_convention = cc_present; params_and_body = _; } as t0)
    | (Present { calling_convention = cc_present; params_and_body = _; } as t0),
      Imported { calling_convention = cc_imported; } ->
      if Calling_convention.equal cc_present cc_imported then Some t0
      else
        Misc.fatal_errorf
          "Code_id %a is present with calling convention %a\
           but imported with calling convention %a"
          Code_id.print code_id
          Calling_convention.print cc_present
          Calling_convention.print cc_imported
  in
  Code_id.Map.union merge_one t1 t2

let mem code_id t =
  Code_id.Map.mem code_id t

let find_code t code_id =
  match Code_id.Map.find code_id t with
  | exception Not_found ->
    Misc.fatal_errorf "Code ID %a not bound" Code_id.print code_id
  | Present { params_and_body; calling_convention = _; } -> params_and_body
  | Imported _ ->
    Misc.fatal_errorf "Actual code for Code ID %a is missing"
      Code_id.print code_id

let find_code_if_not_imported t code_id =
  match Code_id.Map.find code_id t with
  | exception Not_found ->
    Misc.fatal_errorf "Code ID %a not bound" Code_id.print code_id
  | Present { params_and_body; calling_convention = _; } -> Some params_and_body
  | Imported _ ->
    None

let find_calling_convention t code_id =
  match Code_id.Map.find code_id t with
  | exception Not_found ->
    Misc.fatal_errorf "Code ID %a not bound" Code_id.print code_id
  | Present { params_and_body = _; calling_convention; } -> calling_convention
  | Imported { calling_convention; } -> calling_convention

let remove_unreachable t ~reachable_names =
  Code_id.Map.filter (fun code_id _exported_code ->
      Name_occurrences.mem_code_id reachable_names code_id)
    t

let all_ids_for_export t =
  Code_id.Map.fold (fun code_id code_data all_ids ->
      let all_ids = Ids_for_export.add_code_id all_ids code_id in
      match code_data with
      | Present { params_and_body; calling_convention = _; } ->
        Ids_for_export.union all_ids
          (P.all_ids_for_export params_and_body)
      | Imported { calling_convention = _; } -> all_ids)
    t
    Ids_for_export.empty

let import import_map t =
  Code_id.Map.fold (fun code_id code_data all_code ->
      let code_id = Ids_for_export.Import_map.code_id import_map code_id in
      let code_data =
        match code_data with
        | Present { calling_convention; params_and_body; } ->
          let params_and_body =
            Flambda.Function_params_and_body.import import_map params_and_body
          in
          Present { calling_convention; params_and_body; }
        | Imported { calling_convention; } ->
          Imported { calling_convention; }
      in
      Code_id.Map.add code_id code_data all_code)
    t
    Code_id.Map.empty
