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

module I = Target_imm
module T = Type_grammar
module TE = Typing_env
module TEE = Typing_env_extension

type t =
  | Naked_immediates of I.Set.t
  | Is_int of T.t
  | Get_tag of T.t

let print_with_cache ~cache ppf t =
  match t with
  | Naked_immediates is ->
    Format.fprintf ppf "@[<hov 1>(%a)@]" I.Set.print is
  | Is_int ty ->
    Format.fprintf ppf "@[<hov 1>(Is_int@ %a)@]"
      (T.print_with_cache ~cache) ty
  | Get_tag ty ->
    Format.fprintf ppf "@[<hov 1>(Get_tag@ %a)@]"
      (T.print_with_cache ~cache) ty

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let apply_name_permutation t perm =
  match t with
  | Naked_immediates _ -> t
  | Is_int ty ->
    let ty' = T.apply_name_permutation ty perm in
    if ty == ty' then t
    else Is_int ty'
  | Get_tag ty ->
    let ty' = T.apply_name_permutation ty perm in
    if ty == ty' then t
    else Get_tag ty'

let free_names t =
  match t with
  | Naked_immediates _ -> Name_occurrences.empty
  | Is_int ty | Get_tag ty -> T.free_names ty

let all_ids_for_export t =
  match t with
  | Naked_immediates _ -> Ids_for_export.empty
  | Is_int ty | Get_tag ty -> T.all_ids_for_export ty

let import import_map t =
  match t with
  | Naked_immediates _ -> t
  | Is_int ty -> Is_int (T.import import_map ty)
  | Get_tag ty -> Get_tag (T.import import_map ty)

let apply_rec_info t rec_info : _ Or_bottom.t =
  if Rec_info.is_initial rec_info then Ok t
  else Bottom

module Make_meet_or_join
  (E : Lattice_ops_intf.S
    with type meet_env := Meet_env.t
    with type meet_or_join_env := Meet_or_join_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t) =
struct
  let meet env t1 t2 : _ Or_bottom.t =
    match t1, t2 with
    | Naked_immediates is1, Naked_immediates is2 ->
      let is = I.Set.inter is1 is2 in
      if I.Set.is_empty is then Bottom
      else Ok (Naked_immediates is, TEE.empty ())
    | Is_int ty1, Is_int ty2 ->
      Or_bottom.map (T.meet env ty1 ty2)
        ~f:(fun (ty, env_extension) -> Is_int ty, env_extension)
    | Get_tag ty1, Get_tag ty2 ->
      Or_bottom.map (T.meet env ty1 ty2)
        ~f:(fun (ty, env_extension) -> Get_tag ty, env_extension)
    | Is_int ty, Naked_immediates is_int ->
      begin match I.Set.elements is_int with
      | [] -> Bottom
      | [is_int] ->
        let shape =
          if I.equal is_int I.zero then Some (T.any_block ())
          else if I.equal is_int I.one then Some (T.any_tagged_immediate ())
          else None
        in
        begin match shape with
        | Some shape ->
          Or_bottom.map
            (T.meet env ty shape)
            ~f:(fun (ty, env_extension) -> Is_int ty, env_extension)
        | None -> Bottom
        end
      | _ :: _ :: _ ->
        (* Note: we're potentially losing precision because the set could end up
           not containing either 0 or 1 or both, but this should be uncommon. *)
        Ok (Is_int ty, TEE.empty ())
      end
    | Naked_immediates is_int, Is_int ty ->
      begin match I.Set.elements is_int with
      | [] -> Bottom
      | [is_int] ->
        let shape =
          if I.equal is_int I.zero then Some (T.any_block ())
          else if I.equal is_int I.one then Some (T.any_tagged_immediate ())
          else None
        in
        begin match shape with
        | Some shape ->
          Or_bottom.map
            (T.meet env shape ty)
            ~f:(fun (ty, env_extension) -> Is_int ty, env_extension)
        | None -> Bottom
        end
      | _ :: _ :: _ ->
        (* Note: we're potentially losing precision because the set could end up
           not containing either 0 or 1 or both, but this should be uncommon. *)
        Ok (Is_int ty, TEE.empty ())
      end
    | Get_tag ty, Naked_immediates tags ->
      (* CR mshinwell: eliminate code duplication, same above.  Or-patterns
         aren't the answer, since join depends on the left/right envs! *)
      let exception Invalid_tag in
      begin try
        let tags =
          I.Set.fold (fun tag tags ->
              match Target_imm.to_tag tag with
              | Some tag -> Tag.Set.add tag tags
              | None -> raise Invalid_tag)
            tags
            Tag.Set.empty
        in
        begin match T.blocks_with_these_tags tags with
        | Known shape ->
          Or_bottom.map
            (T.meet env ty shape)
            ~f:(fun (ty, env_extension) -> Get_tag ty, env_extension)
        | Unknown ->
          Ok (Get_tag ty, TEE.empty ())
        end
      with Invalid_tag ->
        (* There is a choice to make here: is it more interesting to keep
           the Get_tag type, or the Naked_immediates type ?
           In general the Get_tag type is more interesting, but we reach this
           case only when we're doing a meet with something that cannot be
           a regular tag, so it's likely that the Naked_immediates set is more
           relevant. *)
        let tags =
          I.Set.filter (fun tag ->
              match Tag.create_from_targetint (Target_imm.to_targetint tag) with
              | None -> false
              | Some _ -> true)
            tags
        in
        Ok (Naked_immediates tags, TEE.empty ())
      end
    | (Is_int _ | Get_tag _), (Is_int _ | Get_tag _) ->
      (* We can't return Bottom, as it would be unsound, so we need to either
         do the actual meet with Naked_immediates, or just give up and return
         one of the arguments. *)
      Ok (t1, TEE.empty ())
    | Naked_immediates tags, Get_tag ty ->
      let exception Invalid_tag in
      begin try
        let tags =
          I.Set.fold (fun tag tags ->
              match Target_imm.to_tag tag with
              | Some tag -> Tag.Set.add tag tags
              | None -> raise Invalid_tag)
            tags
            Tag.Set.empty
        in
        begin match T.blocks_with_these_tags tags with
        | Known shape ->
          Or_bottom.map
            (T.meet env ty shape)
            ~f:(fun (ty, env_extension) -> Get_tag ty, env_extension)
        | Unknown ->
          Ok (Get_tag ty, TEE.empty ())
        end
      with Invalid_tag ->
        (* There is a choice to make here: is it more interesting to keep
           the Get_tag type, or the Naked_immediates type ?
           In general the Get_tag type is more interesting, but we reach this
           case only when we're doing a meet with something that cannot be
           a regular tag, so it's likely that the Naked_immediates set is more
           relevant. *)
        let tags =
          I.Set.filter (fun tag ->
              match Tag.create_from_targetint (Target_imm.to_targetint tag) with
              | None -> false
              | Some _ -> true)
            tags
        in
        Ok (Naked_immediates tags, TEE.empty ())
      end

  let all_regular_tags =
    let rec compute_all_tags acc n =
      match Tag.create n with
      | None -> acc
      | Some t ->
        if Tag.is_structured_block_but_not_a_variant t
        then acc
        else begin
          assert (Tag.is_structured_block t);
          let imm =
            Target_imm.int (Targetint.OCaml.of_int n)
          in
          compute_all_tags (I.Set.add imm acc) (succ n)
        end
    in
    compute_all_tags I.Set.empty 0

  let to_naked_immediates t =
    match t with
    | Naked_immediates imms -> imms
    | Is_int _ -> I.all_bools
    | Get_tag _ ->
      (* The current infrastructure around joins doesn't make it easy to
         return Unknown, so we compute the set of all possible tags *)
      I.all_regular_tags

  let join env t1 t2 =
    match t1, t2 with
    | Naked_immediates is1, Naked_immediates is2 ->
      let is = I.Set.union is1 is2 in
      Naked_immediates is
    | Is_int ty1, Is_int ty2 ->
      Is_int (T.join' env ty1 ty2)
    | Get_tag ty1, Get_tag ty2 ->
      Get_tag (T.join' env ty1 ty2)
    | ty, Naked_immediates nimms
    | Naked_immediates nimms, ty ->
      if I.Set.is_empty nimms then ty
      else Naked_immediates (I.Set.union nimms (to_naked_immediates ty))
    | Is_int _, Get_tag _ | Get_tag _, Is_int _ ->
      (* There's a possibility that we could get a more precise result, but again
         it's probably not worth the trouble *)
      Naked_immediates all_regular_tags

  let meet_or_join env t1 t2 =
    Or_bottom_or_absorbing.of_or_bottom (E.switch meet join env t1 t2)
      ~f:(fun x -> x)
end
