(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Singleton of Var_in_binding_pos.t
  | Set_of_closures of {
      name_mode : Name_mode.t;
      closure_vars : Var_in_binding_pos.t Closure_id.Map.t;
    }

include Identifiable.Make (struct
  type nonrec t = t

  let print_closure_binding ppf (closure_id, var) =
    Format.fprintf ppf "@[%a @<0>%s\u{21a4}@<0>%s %a@]"
      Var_in_binding_pos.print var
      (Flambda_colours.elide ())
      (Flambda_colours.elide ())
      Closure_id.print closure_id

  let print ppf t =
    match t with
    | Singleton var -> Var_in_binding_pos.print ppf var
    | Set_of_closures { name_mode = _; closure_vars; } ->
      Format.fprintf ppf "@[<hov 1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          print_closure_binding)
        (Closure_id.Map.bindings closure_vars)

  let compare t1 t2 =
    match t1, t2 with
    | Singleton var1, Singleton var2 -> Var_in_binding_pos.compare var1 var2
    | Singleton _, Set_of_closures _ -> -1
    | Set_of_closures _, Singleton _ -> 1
    | Set_of_closures { name_mode = _;
                        closure_vars = closure_vars1; },
        Set_of_closures { name_mode = _;
                          closure_vars = closure_vars2; } ->
      (* The [name_mode]s are uniquely determined by the
         [closure_vars], so we don't need to compare them. *)
      Closure_id.Map.compare Var_in_binding_pos.compare
        closure_vars1 closure_vars2

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash _ = Misc.fatal_error "Not yet implemented"

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)

let print_with_cache ~cache:_ ppf t = print ppf t

let free_names t =
  match t with
  | Singleton var ->
    let var = Var_in_binding_pos.var var in
    Name_occurrences.singleton_variable var Name_mode.normal
  | Set_of_closures { name_mode = _; closure_vars; } ->
    Closure_id.Map.fold (fun _closure_id var free_names ->
        let var = Var_in_binding_pos.var var in
        Name_occurrences.add_variable free_names var
          Name_mode.normal)
      closure_vars
      Name_occurrences.empty

let apply_name_permutation t perm =
  match t with
  | Singleton var ->
    let var' = Var_in_binding_pos.apply_name_permutation var perm in
    if var == var' then t
    else Singleton var'
  | Set_of_closures { name_mode; closure_vars; } ->
    let closure_vars' =
      Closure_id.Map.map_sharing (fun var ->
          Var_in_binding_pos.apply_name_permutation var perm)
        closure_vars
    in
    if closure_vars == closure_vars' then t
    else Set_of_closures { name_mode; closure_vars = closure_vars'; }

let all_ids_for_export t =
  match t with
  | Singleton var ->
    Ids_for_export.add_variable Ids_for_export.empty
      (Var_in_binding_pos.var var)
  | Set_of_closures { name_mode = _; closure_vars; } ->
    Closure_id.Map.fold (fun _closure_id var ids ->
        Ids_for_export.add_variable ids (Var_in_binding_pos.var var))
      closure_vars
      Ids_for_export.empty

let import import_map t =
  match t with
  | Singleton var ->
    let raw_var =
      Ids_for_export.Import_map.variable import_map (Var_in_binding_pos.var var)
    in
    Singleton
      (Var_in_binding_pos.create raw_var (Var_in_binding_pos.name_mode var))
  | Set_of_closures { name_mode; closure_vars; } ->
    let closure_vars =
      Closure_id.Map.map (fun var ->
          Var_in_binding_pos.create
            (Ids_for_export.Import_map.variable import_map
               (Var_in_binding_pos.var var))
            (Var_in_binding_pos.name_mode var))
        closure_vars
    in
    Set_of_closures { name_mode; closure_vars; }

let rename t =
  match t with
  | Singleton var -> Singleton (Var_in_binding_pos.rename var)
  | Set_of_closures { name_mode; closure_vars; } ->
    let closure_vars =
      Closure_id.Map.map (fun var -> Var_in_binding_pos.rename var) closure_vars
    in
    Set_of_closures { name_mode; closure_vars; }

let add_to_name_permutation t1 ~guaranteed_fresh:t2 perm =
  match t1, t2 with
  | Singleton var1, Singleton var2 ->
    Name_permutation.add_fresh_variable perm
      (Var_in_binding_pos.var var1)
      ~guaranteed_fresh:(Var_in_binding_pos.var var2)
  | Set_of_closures { name_mode = _; closure_vars = closure_vars1; },
      Set_of_closures { name_mode = _; 
        closure_vars = closure_vars2; } ->
    let perm =
      Closure_id.Map.fold2_stop_on_key_mismatch
        (fun _closure_var var1 var2 perm ->
          Name_permutation.add_fresh_variable perm
            (Var_in_binding_pos.var var1)
            ~guaranteed_fresh:(Var_in_binding_pos.var var2))
        closure_vars1
        closure_vars2
        perm
    in
    begin match perm with
    | Some perm -> perm
    | None ->
      Misc.fatal_errorf "Mismatching closure vars:@ %a@ and@ %a"
        print t1
        print t2
    end
  | (Singleton _ | Set_of_closures _), _ ->
    Misc.fatal_errorf "Kind mismatch:@ %a@ and@ %a"
      print t1
      print t2

let name_permutation t ~guaranteed_fresh =
  add_to_name_permutation t ~guaranteed_fresh Name_permutation.empty

let singleton_occurrence_in_terms t = free_names t

let add_occurrence_in_terms t occs =
  Name_occurrences.union (free_names t) occs

let singleton var = Singleton var

let set_of_closures ~closure_vars =
  let name_modes =
    Closure_id.Map.fold (fun _closure_id var name_modes ->
        Name_mode.Set.add (Var_in_binding_pos.name_mode var)
          name_modes)
      closure_vars
      Name_mode.Set.empty
  in
  match Name_mode.Set.elements name_modes with
  | [] -> Misc.fatal_error "No closure IDs provided"
  | [name_mode] ->
    (* CR mshinwell: Check there are no duplicates in [closure_vars] *)
    Set_of_closures {
      name_mode;
      closure_vars;
    }
  | _ ->
    Misc.fatal_errorf "Inconsistent name occurrence kinds:@ %a"
      (Closure_id.Map.print Var_in_binding_pos.print) closure_vars

let name_mode t =
  match t with
  | Singleton var -> Var_in_binding_pos.name_mode var
  | Set_of_closures { name_mode; _ } -> name_mode

let must_be_singleton t =
  match t with
  | Singleton var -> var
  | Set_of_closures _ ->
    Misc.fatal_errorf "Bound name is not a [Singleton]:@ %a" print t

let must_be_set_of_closures t =
  match t with
  | Set_of_closures { closure_vars; _ } -> closure_vars
  | Singleton _ ->
    Misc.fatal_errorf "Bound name is not a [Set_of_closures]:@ %a" print t

let all_bound_vars t =
  match t with
  | Singleton var -> Var_in_binding_pos.Set.singleton var
  | Set_of_closures { closure_vars; _ } ->
    Var_in_binding_pos.Set.of_list (Closure_id.Map.data closure_vars)

let all_bound_vars' t =
  match t with
  | Singleton var -> Variable.Set.singleton (Var_in_binding_pos.var var)
  | Set_of_closures { closure_vars; _ } ->
    Variable.Set.of_list (
      List.map Var_in_binding_pos.var (Closure_id.Map.data closure_vars))
