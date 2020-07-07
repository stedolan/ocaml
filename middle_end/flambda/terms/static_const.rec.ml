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

[@@@ocaml.warning "+a-30-40-41-42"]

let fprintf = Format.fprintf

module K = Flambda_kind

module Field_of_block = struct
  type t =
    | Symbol of Symbol.t
    | Tagged_immediate of Target_imm.t
    | Dynamically_computed of Variable.t

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Symbol s1, Symbol s2 -> Symbol.compare s1 s2
      | Tagged_immediate t1, Tagged_immediate t2 ->
        Target_imm.compare t1 t2
      | Dynamically_computed v1, Dynamically_computed v2 ->
        Variable.compare v1 v2
      | Symbol _, Tagged_immediate _ -> -1
      | Tagged_immediate _, Symbol _ -> 1
      | Symbol _, Dynamically_computed _ -> -1
      | Dynamically_computed _, Symbol _ -> 1
      | Tagged_immediate _, Dynamically_computed _ -> -1
      | Dynamically_computed _, Tagged_immediate _ -> 1

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash t =
      match t with
      | Symbol symbol -> Hashtbl.hash (0, Symbol.hash symbol)
      | Tagged_immediate immediate ->
        Hashtbl.hash (1, Target_imm.hash immediate)
      | Dynamically_computed var -> Hashtbl.hash (2, Variable.hash var)

    let print ppf t =
      match t with
      | Symbol symbol -> Symbol.print ppf symbol
      | Tagged_immediate immediate -> Target_imm.print ppf immediate
      | Dynamically_computed var -> Variable.print ppf var

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  let apply_name_permutation t perm =
    match t with
    | Symbol _ | Tagged_immediate _ -> t
    | Dynamically_computed var ->
      let var' = Name_permutation.apply_variable perm var in
      if var == var' then t
      else Dynamically_computed var'

  let free_names t =
    match t with
    | Dynamically_computed var ->
      Name_occurrences.singleton_variable var Name_mode.normal
    | Symbol sym ->
      Name_occurrences.singleton_symbol sym Name_mode.normal
    | Tagged_immediate _ -> Name_occurrences.empty

  let all_ids_for_export t =
    match t with
    | Dynamically_computed var ->
      Ids_for_export.add_variable Ids_for_export.empty var
    | Symbol sym ->
      Ids_for_export.add_symbol Ids_for_export.empty sym
    | Tagged_immediate _ -> Ids_for_export.empty

  let import import_map t =
    match t with
    | Dynamically_computed var ->
      let var = Ids_for_export.Import_map.variable import_map var in
      Dynamically_computed var
    | Symbol sym ->
      let sym = Ids_for_export.Import_map.symbol import_map sym in
      Symbol sym
    | Tagged_immediate _ -> t

(*
  let invariant env t =
    let module E = Invariant_env in
    match t with
    | Symbol sym -> E.check_symbol_is_bound env sym
    | Tagged_immediate _ -> ()
    | Dynamically_computed var ->
      E.check_variable_is_bound_and_of_kind env var K.value
*)
end

module Code = struct
  type t = {
    params_and_body : Function_params_and_body.t or_deleted;
    newer_version_of : Code_id.t option;
  }
  and 'a or_deleted =
    | Present of 'a
    | Deleted

  let print_params_and_body_with_cache ~cache ppf params_and_body =
    match params_and_body with
    | Deleted -> Format.fprintf ppf "Deleted"
    | Present params_and_body ->
      Function_params_and_body.print_with_cache ~cache ppf
        params_and_body

  let print_with_cache ~cache ppf { params_and_body; newer_version_of; } =
    Format.fprintf ppf "@[<v 1>(\
        @[<hov 1>@<0>%s(newer_version_of@ %a)@<0>%s@]@ \
        %a\
        )@]"
      (if Option.is_none newer_version_of then Flambda_colours.elide ()
       else Flambda_colours.normal ())
      (Misc.Stdlib.Option.print_compact Code_id.print) newer_version_of
      (Flambda_colours.normal ())
      (print_params_and_body_with_cache ~cache) params_and_body

  let print ppf code =
    print_with_cache ~cache:(Printing_cache.create ()) ppf code

  let free_names { params_and_body; newer_version_of; } =
    let from_newer_version_of =
      match newer_version_of with
      | None -> Name_occurrences.empty
      | Some older ->
        Name_occurrences.add_newer_version_of_code_id
          Name_occurrences.empty older Name_mode.normal
    in
    let from_params_and_body =
      match params_and_body with
      | Deleted -> Name_occurrences.empty
      | Present params_and_body ->
        Function_params_and_body.free_names params_and_body
    in
    Name_occurrences.union from_newer_version_of from_params_and_body

  let apply_name_permutation ({ params_and_body; newer_version_of; } as t)
        perm =
    let params_and_body' =
      match params_and_body with
      | Deleted -> Deleted
      | Present params_and_body_inner ->
        let params_and_body_inner' =
          Function_params_and_body.apply_name_permutation
            params_and_body_inner perm
        in
        if params_and_body_inner == params_and_body_inner' then
          params_and_body
        else
          Present params_and_body_inner'
    in
    if params_and_body == params_and_body' then t
    else
      { params_and_body = params_and_body';
        newer_version_of;
      }

  let all_ids_for_export { params_and_body; newer_version_of; } =
    let newer_version_of_ids =
      match newer_version_of with
      | None -> Ids_for_export.empty
      | Some older ->
        Ids_for_export.add_code_id Ids_for_export.empty older
    in
    let params_and_body_ids =
      match params_and_body with
      | Deleted -> Ids_for_export.empty
      | Present params_and_body ->
        Function_params_and_body.all_ids_for_export params_and_body
    in
    Ids_for_export.union newer_version_of_ids params_and_body_ids

  let import import_map { params_and_body; newer_version_of; } =
    let params_and_body =
      match params_and_body with
      | Deleted -> Deleted
      | Present params_and_body ->
        let params_and_body =
          Function_params_and_body.import import_map params_and_body
        in
        Present params_and_body
    in
    let newer_version_of =
      match newer_version_of with
      | None -> None
      | Some older ->
        let older = Ids_for_export.Import_map.code_id import_map older in
        Some older
    in
    { params_and_body; newer_version_of; }

  let make_deleted t =
    { params_and_body = Deleted;
      newer_version_of = t.newer_version_of;
    }
end

module Code_and_set_of_closures = struct
  type t = {
    code : Code.t Code_id.Lmap.t;
    set_of_closures : Set_of_closures.t;
  }

  let print_with_cache ~cache ppf { code; set_of_closures; } =
    if Set_of_closures.is_empty set_of_closures then
      match Code_id.Lmap.get_singleton code with
      | None ->
        fprintf ppf "@[<hov 1>(@<0>%sCode@<0>%s@ (\
            @[<hov 1>%a@]\
            ))@]"
          (Flambda_colours.static_part ())
          (Flambda_colours.normal ())
          (Code_id.Lmap.print (Code.print_with_cache ~cache)) code
      | Some (_code_id, code) ->
        fprintf ppf "@[<hov 1>(@<0>%sCode@<0>%s@ (\
            @[<hov 1>%a@]\
            ))@]"
          (Flambda_colours.static_part ())
          (Flambda_colours.normal ())
          (Code.print_with_cache ~cache) code
    else
      fprintf ppf "@[<hov 1>(@<0>%sSets_of_closures@<0>%s@ (\
          @[<hov 1>(code@ (%a))@]@ \
          @[<hov 1>(set_of_closures@ (%a))@]\
          ))@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        (Code_id.Lmap.print (Code.print_with_cache ~cache)) code
        (Set_of_closures.print_with_cache ~cache) set_of_closures

  let compare { code = code1; set_of_closures = set1; }
        { code = code2; set_of_closures = set2; } =
    let c =
      let id_set code =
        Code_id.Set.of_list (Code_id.Lmap.keys code)
      in
      Code_id.Set.compare (id_set code1) (id_set code2)
    in
    if c <> 0 then c
    else Set_of_closures.compare set1 set2

  let free_names { code; set_of_closures; } =
    Code_id.Lmap.fold (fun _code_id code free_names ->
        Name_occurrences.union_list [
          (* [code_id] does not count as a free name. *)
          Code.free_names code;
          free_names;
        ])
      code
      (Set_of_closures.free_names set_of_closures)

  let apply_name_permutation ({ code; set_of_closures; } as t) perm =
    let code' =
      Code_id.Lmap.map_sharing
        (fun code -> Code.apply_name_permutation code perm)
        code
    in
    let set_of_closures' =
      let set' =
        Set_of_closures.apply_name_permutation set_of_closures perm
      in
      if set_of_closures == set' then set_of_closures
      else set'
    in
    if code == code' && set_of_closures == set_of_closures' then t
    else
      { code = code';
        set_of_closures = set_of_closures';
      }

  let all_ids_for_export { code; set_of_closures; } =
    let set_of_closures_ids =
      Set_of_closures.all_ids_for_export set_of_closures
    in
    Code_id.Lmap.fold (fun code_id code ids ->
        Ids_for_export.union ids
          (Ids_for_export.add_code_id (Code.all_ids_for_export code) code_id))
      code
      set_of_closures_ids

  let import import_map { code; set_of_closures; } =
    let code =
      Code_id.Lmap.bindings code
      |> List.map (fun (code_id, code) ->
          let code_id = Ids_for_export.Import_map.code_id import_map code_id in
          let code = Code.import import_map code in
          code_id, code)
      |> Code_id.Lmap.of_list
    in
    let set_of_closures =
      Set_of_closures.import import_map set_of_closures
    in
    { code; set_of_closures; }

  let map_code t ~f =
    { code = Code_id.Lmap.mapi f t.code;
      set_of_closures = t.set_of_closures;
    }
end

type t =
  | Block of Tag.Scannable.t * Mutability.t * (Field_of_block.t list)
  | Sets_of_closures of Code_and_set_of_closures.t list
  | Boxed_float of Numbers.Float_by_bit_pattern.t Or_variable.t
  | Boxed_int32 of Int32.t Or_variable.t
  | Boxed_int64 of Int64.t Or_variable.t
  | Boxed_nativeint of Targetint.t Or_variable.t
  | Immutable_float_block of Numbers.Float_by_bit_pattern.t Or_variable.t list
  | Immutable_float_array of Numbers.Float_by_bit_pattern.t Or_variable.t list
  | Mutable_string of { initial_value : string; }
  | Immutable_string of string

include Identifiable.Make (struct
  type nonrec t = t

  let print_with_cache ~cache ppf t =
    match t with
    | Block (tag, mut, fields) ->
      fprintf ppf "@[<hov 1>(@<0>%s%sblock@<0>%s@ (tag %a)@ (%a))@]"
        (Flambda_colours.static_part ())
        (match mut with
         | Immutable -> "Immutable_"
         | Immutable_unique -> "Unique_"
         | Mutable -> "Mutable_")
        (Flambda_colours.normal ())
        Tag.Scannable.print tag
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          Field_of_block.print) fields
    | Sets_of_closures sets ->
      Format.pp_print_list ~pp_sep:Format.pp_print_space
        (Code_and_set_of_closures.print_with_cache ~cache) ppf sets
    | Boxed_float or_var ->
      fprintf ppf "@[<hov 1>(@<0>%sBoxed_float@<0>%s@ %a)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        (Or_variable.print Numbers.Float_by_bit_pattern.print) or_var
    | Boxed_int32 or_var ->
      fprintf ppf "@[<hov 1>(@<0>%sBoxed_int32@<0>%s@ %a)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        (Or_variable.print Numbers.Int32.print) or_var
    | Boxed_int64 or_var ->
      fprintf ppf "@[<hov 1>(@<0>%sBoxed_int64@<0>%s@ %a)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        (Or_variable.print Numbers.Int64.print) or_var
    | Boxed_nativeint or_var ->
      fprintf ppf "@[<hov 1>(@<0>%sBoxed_nativeint@<0>%s@ %a)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        (Or_variable.print Targetint.print) or_var
    | Immutable_float_block fields ->
      fprintf ppf "@[<hov 1>(@<0>%sImmutable_float_block@<0>%s@ @[[| %a |]@])@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@; ")
          (Or_variable.print Numbers.Float_by_bit_pattern.print))
        fields
    | Immutable_float_array fields ->
      fprintf ppf "@[<hov 1>(@<0>%sImmutable_float_array@<0>%s@ @[[| %a |]@])@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@; ")
          (Or_variable.print Numbers.Float_by_bit_pattern.print))
        fields
    | Mutable_string { initial_value = s; } ->
      fprintf ppf "@[<hov 1>(@<0>%sMutable_string@<0>%s@ %S)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        s
    | Immutable_string s ->
      fprintf ppf "@[<hov 1>(@<0>%sImmutable_string@<0>%s@ %S)@]"
        (Flambda_colours.static_part ())
        (Flambda_colours.normal ())
        s

  let print ppf t =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let compare t1 t2 =
    match t1, t2 with
    | Block (tag1, mut1, fields1), Block (tag2, mut2, fields2) ->
      let c = Tag.Scannable.compare tag1 tag2 in
      if c <> 0 then c
      else
        let c = Mutability.compare mut1 mut2 in
        if c <> 0 then c
        else Misc.Stdlib.List.compare Field_of_block.compare fields1 fields2
    | Sets_of_closures sets1, Sets_of_closures sets2 ->
      Misc.Stdlib.List.compare Code_and_set_of_closures.compare sets1 sets2
    | Boxed_float or_var1, Boxed_float or_var2 ->
      Or_variable.compare Numbers.Float_by_bit_pattern.compare or_var1 or_var2
    | Boxed_int32 or_var1, Boxed_int32 or_var2 ->
      Or_variable.compare Numbers.Int32.compare or_var1 or_var2
    | Boxed_int64 or_var1, Boxed_int64 or_var2 ->
      Or_variable.compare Numbers.Int64.compare or_var1 or_var2
    | Boxed_nativeint or_var1, Boxed_nativeint or_var2 ->
      Or_variable.compare Targetint.compare or_var1 or_var2
    | Immutable_float_block fields1, Immutable_float_array fields2 ->
      Misc.Stdlib.List.compare
        (Or_variable.compare Numbers.Float_by_bit_pattern.compare)
        fields1 fields2
    | Immutable_float_array fields1, Immutable_float_array fields2 ->
      Misc.Stdlib.List.compare
        (Or_variable.compare Numbers.Float_by_bit_pattern.compare)
        fields1 fields2
    | Mutable_string { initial_value = s1; },
      Mutable_string { initial_value = s2; }
    | Immutable_string s1, Immutable_string s2 -> String.compare s1 s2
    | Block _, _ -> -1
    | _, Block _ -> 1
    | Sets_of_closures _, _ -> -1
    | _, Sets_of_closures _ -> 1
    | Boxed_float _, _ -> -1
    | _, Boxed_float _ -> 1
    | Boxed_int32 _, _ -> -1
    | _, Boxed_int32 _ -> 1
    | Boxed_int64 _, _ -> -1
    | _, Boxed_int64 _ -> 1
    | Boxed_nativeint _, _ -> -1
    | _, Boxed_nativeint _ -> 1
    | Immutable_float_block _, _ -> -1
    | _, Immutable_float_block _ -> 1
    | Immutable_float_array _, _ -> -1
    | _, Immutable_float_array _ -> 1
    | Mutable_string _, _ -> -1
    | Immutable_string _, Mutable_string _ -> 1

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash _t = Misc.fatal_error "Not yet implemented"

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)

let get_pieces_of_code t =
  match t with
  | Sets_of_closures sets ->
    Code_id.Lmap.disjoint_union_many
      (List.map
        (fun ({ code; set_of_closures = _; } : Code_and_set_of_closures.t) ->
          code)
        sets)
  | Block _
  | Boxed_float _
  | Boxed_int32 _
  | Boxed_int64 _
  | Boxed_nativeint _
  | Immutable_float_block _
  | Immutable_float_array _
  | Mutable_string _
  | Immutable_string _ -> Code_id.Lmap.empty

let get_pieces_of_code' t =
  Code_id.Lmap.filter_map (get_pieces_of_code t)
    ~f:(fun _ code ->
      match code.Code.params_and_body with
      | Deleted -> None
      | Present params_and_body -> Some params_and_body)

let free_names t =
  match t with
  | Block (_tag, _mut, fields) ->
    List.fold_left (fun fvs field ->
        Name_occurrences.union fvs (Field_of_block.free_names field))
      (Name_occurrences.empty)
      fields
  | Sets_of_closures sets ->
    let free_names_list = List.map Code_and_set_of_closures.free_names sets in
    Name_occurrences.union_list free_names_list
  | Boxed_float or_var -> Or_variable.free_names or_var
  | Boxed_int32 or_var -> Or_variable.free_names or_var
  | Boxed_int64 or_var -> Or_variable.free_names or_var
  | Boxed_nativeint or_var -> Or_variable.free_names or_var
  | Mutable_string { initial_value = _; }
  | Immutable_string _ -> Name_occurrences.empty
  | Immutable_float_block fields | Immutable_float_array fields ->
    List.fold_left (fun fns (field : _ Or_variable.t) ->
        match field with
        | Var v ->
          Name_occurrences.add_variable fns v Name_mode.normal
        | Const _ -> fns)
      (Name_occurrences.empty)
      fields

let apply_name_permutation t perm =
  if Name_permutation.is_empty perm then t
  else
    match t with
    | Block (tag, mut, fields) ->
      let changed = ref false in
      let fields =
        List.map (fun field ->
            let field' = Field_of_block.apply_name_permutation field perm in
            if not (field == field') then begin
              changed := true
            end;
            field')
          fields
      in
      if not !changed then t
      else Block (tag, mut, fields)
    | Sets_of_closures sets ->
      let sets' =
        List.map (fun set ->
            Code_and_set_of_closures.apply_name_permutation set perm)
          sets
      in
      if List.for_all2 (fun set1 set2 -> set1 == set2) sets sets' then t
      else Sets_of_closures sets'
    | Boxed_float or_var ->
      let or_var' = Or_variable.apply_name_permutation or_var perm in
      if or_var == or_var' then t
      else Boxed_float or_var'
    | Boxed_int32 or_var ->
      let or_var' = Or_variable.apply_name_permutation or_var perm in
      if or_var == or_var' then t
      else Boxed_int32 or_var'
    | Boxed_int64 or_var ->
      let or_var' = Or_variable.apply_name_permutation or_var perm in
      if or_var == or_var' then t
      else Boxed_int64 or_var'
    | Boxed_nativeint or_var ->
      let or_var' = Or_variable.apply_name_permutation or_var perm in
      if or_var == or_var' then t
      else Boxed_nativeint or_var'
    | Mutable_string { initial_value = _; }
    | Immutable_string _ -> t
    | Immutable_float_block fields ->
      let changed = ref false in
      let fields =
        List.map (fun (field : _ Or_variable.t) ->
            let field' : _ Or_variable.t =
              match field with
              | Var v -> Var (Name_permutation.apply_variable perm v)
              | Const _ -> field
            in
            if not (field == field') then begin
              changed := true
            end;
            field')
          fields
      in
      if not !changed then t
      else Immutable_float_block fields
    | Immutable_float_array fields ->
      let changed = ref false in
      let fields =
        List.map (fun (field : _ Or_variable.t) ->
            let field' : _ Or_variable.t =
              match field with
              | Var v -> Var (Name_permutation.apply_variable perm v)
              | Const _ -> field
            in
            if not (field == field') then begin
              changed := true
            end;
            field')
          fields
      in
      if not !changed then t
      else Immutable_float_array fields

let all_ids_for_export t =
  match t with
  | Block (_tag, _mut, fields) ->
    List.fold_left (fun ids field ->
        Ids_for_export.union ids (Field_of_block.all_ids_for_export field))
      Ids_for_export.empty
      fields
  | Sets_of_closures sets ->
    List.fold_left (fun ids set ->
        Ids_for_export.union ids
          (Code_and_set_of_closures.all_ids_for_export set))
      Ids_for_export.empty
      sets
  | Boxed_float (Var var)
  | Boxed_int32 (Var var)
  | Boxed_int64 (Var var)
  | Boxed_nativeint (Var var) ->
    Ids_for_export.add_variable Ids_for_export.empty var
  | Boxed_float (Const _)
  | Boxed_int32 (Const _)
  | Boxed_int64 (Const _)
  | Boxed_nativeint (Const _)
  | Mutable_string { initial_value = _; }
  | Immutable_string _ -> Ids_for_export.empty
  | Immutable_float_block fields ->
    List.fold_left (fun ids (field : _ Or_variable.t) ->
        match field with
        | Var v ->
          Ids_for_export.add_variable ids v
        | Const _ -> ids)
      Ids_for_export.empty
      fields
  | Immutable_float_array fields ->
    List.fold_left (fun ids (field : _ Or_variable.t) ->
        match field with
        | Var v ->
          Ids_for_export.add_variable ids v
        | Const _ -> ids)
      Ids_for_export.empty
      fields

let import import_map t = match t with
  | Block (tag, mut, fields) ->
    let fields = List.map (Field_of_block.import import_map) fields in
    Block (tag, mut, fields)
  | Sets_of_closures sets ->
    let sets = List.map (Code_and_set_of_closures.import import_map) sets in
    Sets_of_closures sets
  | Boxed_float (Var var) ->
    let var = Ids_for_export.Import_map.variable import_map var in
    Boxed_float (Var var)
  | Boxed_int32 (Var var) ->
    let var = Ids_for_export.Import_map.variable import_map var in
    Boxed_int32 (Var var)
  | Boxed_int64 (Var var) ->
    let var = Ids_for_export.Import_map.variable import_map var in
    Boxed_int64 (Var var)
  | Boxed_nativeint (Var var) ->
    let var = Ids_for_export.Import_map.variable import_map var in
    Boxed_nativeint (Var var)
  | Boxed_float (Const _)
  | Boxed_int32 (Const _)
  | Boxed_int64 (Const _)
  | Boxed_nativeint (Const _)
  | Mutable_string { initial_value = _; }
  | Immutable_string _ -> t
  | Immutable_float_block fields ->
    let fields =
      List.map (fun (field : _ Or_variable.t) : _ Or_variable.t ->
          match field with
          | Const _ -> field
          | Var v -> Var (Ids_for_export.Import_map.variable import_map v))
        fields
    in
    Immutable_float_block fields
  | Immutable_float_array fields ->
    let fields =
      List.map (fun (field : _ Or_variable.t) : _ Or_variable.t ->
          match field with
          | Const _ -> field
          | Var v -> Var (Ids_for_export.Import_map.variable import_map v))
        fields
    in
    Immutable_float_array fields

let is_fully_static t =
  free_names t
  |> Name_occurrences.no_variables

let can_share0 t =
  match t with
  | Block (_, Immutable, _)
  | Sets_of_closures _
  | Boxed_float _
  | Boxed_int32 _
  | Boxed_int64 _
  | Boxed_nativeint _
  | Immutable_float_block _
  | Immutable_float_array _
  | Immutable_string _ -> true
  | Block (_, (Mutable | Immutable_unique), _)
  | Mutable_string _ -> false

let can_share t =
  can_share0 t && is_fully_static t

let must_be_sets_of_closures t =
  match t with
  | Sets_of_closures sets -> sets
  | Block _
  | Boxed_float _
  | Boxed_int32 _
  | Boxed_int64 _
  | Boxed_nativeint _
  | Immutable_float_block _
  | Immutable_float_array _
  | Immutable_string _
  | Mutable_string _ -> Misc.fatal_errorf "Not set(s) of closures:@ %a" print t
