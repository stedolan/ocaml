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

module T = Type_grammar
module TEE = Typing_env_extension

module Blocks = Row_like.For_blocks

type t =
  | Variant of Variant.t
  (* CR mshinwell: Add constructors for the following too so we can check
     they aren't bottom? *)
  | Boxed_float of T.t
  | Boxed_int32 of T.t
  | Boxed_int64 of T.t
  | Boxed_nativeint of T.t
  | Closures of {
      by_closure_id : Row_like.For_closures_entry_by_set_of_closures_contents.t;
    }
  | String of String_info.Set.t
  | Array of { length : T.t; }

let print_with_cache ~cache ppf t =
  match t with
  | Variant { blocks; immediates; } ->
    (* CR mshinwell: Improve so that we elide blocks and/or immediates when
       they're empty. *)
    Format.fprintf ppf
      "@[<hov 1>(Variant@ \
        @[<hov 1>(blocks@ %a)@]@ \
        @[<hov 1>(tagged_imms@ %a)@]\
        )@]"
      (Or_unknown.print (Blocks.print_with_cache ~cache)) blocks
      (Or_unknown.print (T.print_with_cache ~cache)) immediates
  | Boxed_float naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_float@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Boxed_int32 naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_int32@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Boxed_int64 naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_int64@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Boxed_nativeint naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_nativeint@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Closures { by_closure_id; } ->
    Row_like.For_closures_entry_by_set_of_closures_contents.print_with_cache ~cache
      ppf by_closure_id
  | String str_infos ->
    Format.fprintf ppf "@[<hov 1>(Strings@ (%a))@]"
      String_info.Set.print str_infos
  | Array { length; } ->
    Format.fprintf ppf "@[<hov 1>(Array@ (length@ %a))@]"
      (T.print_with_cache ~cache) length

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let apply_name_permutation_variant blocks immediates perm =
  let immediates' =
    Or_unknown.map immediates ~f:(fun immediates ->
      T.apply_name_permutation immediates perm)
  in
  let blocks' =
    Or_unknown.map blocks ~f:(fun blocks ->
      Blocks.apply_name_permutation blocks perm)
  in
  if immediates == immediates' && blocks == blocks' then
    None
  else
    Some (blocks', immediates')

let apply_name_permutation t perm =
  match t with
  | Variant { blocks; immediates; } ->
    begin match
      apply_name_permutation_variant blocks immediates perm
    with
    | None -> t
    | Some (blocks, immediates) ->
      Variant (Variant.create ~blocks ~immediates)
    end
  | Boxed_float ty ->
    let ty' = T.apply_name_permutation ty perm in
    if ty == ty' then t
    else Boxed_float ty'
  | Boxed_int32 ty ->
    let ty' = T.apply_name_permutation ty perm in
    if ty == ty' then t
    else Boxed_int32 ty'
  | Boxed_int64 ty ->
    let ty' = T.apply_name_permutation ty perm in
    if ty == ty' then t
    else Boxed_int64 ty'
  | Boxed_nativeint ty ->
    let ty' = T.apply_name_permutation ty perm in
    if ty == ty' then t
    else Boxed_nativeint ty'
  | Closures { by_closure_id; } ->
    let by_closure_id' =
      Row_like.For_closures_entry_by_set_of_closures_contents.apply_name_permutation
        by_closure_id perm
    in
    if by_closure_id == by_closure_id' then t
    else Closures { by_closure_id = by_closure_id'; }
  | String _ -> t
  | Array { length; } ->
    let length' = T.apply_name_permutation length perm in
    if length == length' then t
    else Array { length = length'; }

let free_names t =
  match t with
  | Variant { blocks; immediates; } ->
    Name_occurrences.union
      (Or_unknown.free_names Blocks.free_names blocks)
      (Or_unknown.free_names T.free_names immediates)
  | Boxed_float ty -> T.free_names ty
  | Boxed_int32 ty -> T.free_names ty
  | Boxed_int64 ty -> T.free_names ty
  | Boxed_nativeint ty -> T.free_names ty
  | Closures { by_closure_id; } ->
    Row_like.For_closures_entry_by_set_of_closures_contents.free_names by_closure_id
  | String _ -> Name_occurrences.empty
  | Array { length; } -> T.free_names length

let all_ids_for_export t =
  match t with
  | Variant { blocks; immediates; } ->
    Ids_for_export.union
      (Or_unknown.all_ids_for_export Blocks.all_ids_for_export blocks)
      (Or_unknown.all_ids_for_export T.all_ids_for_export immediates)
  | Boxed_float ty -> T.all_ids_for_export ty
  | Boxed_int32 ty -> T.all_ids_for_export ty
  | Boxed_int64 ty -> T.all_ids_for_export ty
  | Boxed_nativeint ty -> T.all_ids_for_export ty
  | Closures { by_closure_id; } ->
    Row_like.For_closures_entry_by_set_of_closures_contents.all_ids_for_export
      by_closure_id
  | String _ -> Ids_for_export.empty
  | Array { length; } -> T.all_ids_for_export length

let import import_map t =
  match t with
  | Variant { blocks; immediates; } ->
    let blocks = Or_unknown.import Blocks.import import_map blocks in
    let immediates = Or_unknown.import T.import import_map immediates in
    Variant (Variant.create ~blocks ~immediates)
  | Boxed_float ty -> Boxed_float (T.import import_map ty)
  | Boxed_int32 ty -> Boxed_int32 (T.import import_map ty)
  | Boxed_int64 ty -> Boxed_int64 (T.import import_map ty)
  | Boxed_nativeint ty -> Boxed_nativeint (T.import import_map ty)
  | Closures { by_closure_id; } ->
    let by_closure_id =
      Row_like.For_closures_entry_by_set_of_closures_contents.import import_map
        by_closure_id
    in
    Closures { by_closure_id; }
  | (String _) as t -> t
  | Array { length; } -> Array { length = T.import import_map length; }

let apply_rec_info t rec_info : _ Or_bottom.t =
  match t with
  | Closures { by_closure_id; } ->
    Or_bottom.map
      (Row_like.For_closures_entry_by_set_of_closures_contents.
       map_function_decl_types
        by_closure_id
        ~f:(fun (decl : Function_declaration_type.t) : _ Or_bottom.t ->
          Function_declaration_type.apply_rec_info decl rec_info))
      ~f:(fun by_closure_id -> Closures { by_closure_id; })
  | Variant _
  | Boxed_float _
  | Boxed_int32 _
  | Boxed_int64 _
  | Boxed_nativeint _
  | String _
  | Array _ ->
    if Rec_info.is_initial rec_info then Ok t
    else Bottom

let meet_unknown meet_contents ~contents_is_bottom env
    (or_unknown1 : _ Or_unknown.t) (or_unknown2 : _ Or_unknown.t)
    : ((_ Or_unknown.t) * TEE.t) Or_bottom.t =
  match or_unknown1, or_unknown2 with
  | Unknown, Unknown -> Ok (Unknown, TEE.empty ())
  (* CR mshinwell: Think about the next two cases more *)
  | Known contents, _ when contents_is_bottom contents -> Bottom
  | _, Known contents when contents_is_bottom contents -> Bottom
  | _, Unknown -> Ok (or_unknown1, TEE.empty ())
  | Unknown, _ -> Ok (or_unknown2, TEE.empty ())
  | Known contents1, Known contents2 ->
    let result =
      Or_bottom.map (meet_contents env contents1 contents2)
        ~f:(fun (contents, env_extension) ->
          Or_unknown.Known contents, env_extension)
    in
    match result with
    | Bottom | Ok (Unknown, _) -> result
    | Ok (Known contents, _env_extension) ->
      (* XXX Why isn't [meet_contents] returning bottom? *)
      if contents_is_bottom contents then Bottom
      else result

let join_unknown join_contents (env : Meet_or_join_env.t)
    (or_unknown1 : _ Or_unknown.t) (or_unknown2 : _ Or_unknown.t)
    : _ Or_unknown.t =
  match or_unknown1, or_unknown2 with
  | _, Unknown
  | Unknown, _ -> Unknown
  | Known contents1, Known contents2 ->
    Known (join_contents env contents1 contents2)

module Make_meet_or_join
  (E : Lattice_ops_intf.S
   with type meet_env := Meet_env.t
   with type meet_or_join_env := Meet_or_join_env.t
   with type typing_env := Typing_env.t
   with type typing_env_extension := Typing_env_extension.t) =
struct
  let meet_or_join_variant env
        ~blocks1 ~imms1 ~blocks2 ~imms2 : _ Or_bottom.t =
    let blocks =
      E.switch (meet_unknown Blocks.meet ~contents_is_bottom:Blocks.is_bottom)
        (join_unknown Blocks.join)
        env blocks1 blocks2
    in
    let blocks : _ Or_bottom.t =
      (* XXX Clean this up *)
      match blocks with
      | Bottom | Ok (Or_unknown.Unknown, _) -> blocks
      | Ok (Or_unknown.Known blocks', _) ->
        if Blocks.is_bottom blocks' then Bottom else blocks
    in
    let imms =
      E.switch (meet_unknown T.meet ~contents_is_bottom:T.is_obviously_bottom)
        (join_unknown T.join') env imms1 imms2
    in
    let imms : _ Or_bottom.t =
      match imms with
      | Bottom | Ok (Or_unknown.Unknown, _) -> imms
      | Ok (Or_unknown.Known imms', _) ->
        if T.is_obviously_bottom imms' then Bottom else imms
    in
    match blocks, imms with
    | Bottom, Bottom -> Bottom
    | Ok (blocks, env_extension), Bottom ->
      let immediates : _ Or_unknown.t = Known (T.bottom K.naked_immediate) in
      Ok (blocks, immediates, env_extension)
    | Bottom, Ok (immediates, env_extension) ->
      let blocks : _ Or_unknown.t = Known (Blocks.create_bottom ()) in
      Ok (blocks, immediates, env_extension)
    | Ok (blocks, env_extension1), Ok (immediates, env_extension2) ->
      begin match (blocks : _ Or_unknown.t) with
      | Unknown -> ()
      | Known blocks -> assert (not (Blocks.is_bottom blocks));
      end;
      begin match (immediates : _ Or_unknown.t) with
      | Unknown -> ()
      | Known imms -> assert (not (T.is_obviously_bottom imms));
      end;
      let env_extension = TEE.join env ~params:[] env_extension1 env_extension2 in
      Ok (blocks, immediates, env_extension)

  let meet_or_join env t1 t2 : _ Or_bottom_or_absorbing.t =
    match t1, t2 with
    | Variant { blocks = blocks1; immediates = imms1; },
      Variant { blocks = blocks2; immediates = imms2; } ->
      Or_bottom_or_absorbing.of_or_bottom
        (meet_or_join_variant env ~blocks1 ~imms1 ~blocks2 ~imms2)
        ~f:(fun (blocks, immediates, env_extension) ->
          Variant (Variant.create ~blocks ~immediates), env_extension)
    | Boxed_float n1, Boxed_float n2 ->
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch T.meet T.join' env n1 n2)
        ~f:(fun (n, env_extension) -> Boxed_float n, env_extension)
    | Boxed_int32 n1, Boxed_int32 n2 ->
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch T.meet T.join' env n1 n2)
        ~f:(fun (n, env_extension) -> Boxed_int32 n, env_extension)
    | Boxed_int64 n1, Boxed_int64 n2 ->
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch T.meet T.join' env n1 n2)
        ~f:(fun (n, env_extension) -> Boxed_int64 n, env_extension)
    | Boxed_nativeint n1, Boxed_nativeint n2 ->
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch T.meet T.join' env n1 n2)
        ~f:(fun (n, env_extension) -> Boxed_nativeint n, env_extension)
    | Closures { by_closure_id = by_closure_id1; },
        Closures { by_closure_id = by_closure_id2; } ->
      let module C = Row_like.For_closures_entry_by_set_of_closures_contents in
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch C.meet C.join env by_closure_id1 by_closure_id2)
        ~f:(fun (by_closure_id, env_extension) ->
          Closures { by_closure_id; }, env_extension)
    | String strs1, String strs2 ->
      let strs = E.String_info.Set.union_or_inter strs1 strs2 in
      if String_info.Set.is_empty strs then Bottom
      else Or_bottom_or_absorbing.Ok (String strs, TEE.empty ())
    | Array { length = length1; }, Array { length = length2; } ->
      Or_bottom_or_absorbing.of_or_bottom
        (E.switch T.meet T.join' env length1 length2)
        ~f:(fun (length, env_extension) -> Array { length; }, env_extension)
    | (Variant _
        | Boxed_float _
        | Boxed_int32 _
        | Boxed_int64 _
        | Boxed_nativeint _
        | Closures _
        | String _
        | Array _), _ -> Absorbing
end
