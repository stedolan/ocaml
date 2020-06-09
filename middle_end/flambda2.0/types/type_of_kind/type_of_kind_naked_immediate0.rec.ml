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
  let bad_meet_or_join env t1 t2 =
    Misc.fatal_errorf "Bad naked immediate meet/join with %a and %a in env:@ %a"
      print t1
      print t2
      TE.print (Meet_or_join_env.target_join_env env)

  let meet_or_join env t1 t2 : _ Or_bottom_or_absorbing.t =
  (*
    Format.eprintf "NN meet_or_join %a and %a\n%!"
      print t1 print t2 (*Typing_env.print (Meet_env.env env)*);
      *)
    match t1, t2 with
    | Naked_immediates is1, Naked_immediates is2 ->
      let is = E.Target_imm.Set.union_or_inter is1 is2 in
      if I.Set.is_empty is then Bottom
      else Ok (Naked_immediates is, TEE.empty ())
    | Is_int ty1, Is_int ty2 ->
      Or_bottom_or_absorbing.of_or_bottom (E.switch T.meet T.join' env ty1 ty2)
        ~f:(fun (ty, env_extension) -> Is_int ty, env_extension)
    | Get_tag ty1, Get_tag ty2 ->
      Or_bottom_or_absorbing.of_or_bottom (E.switch T.meet T.join' env ty1 ty2)
        ~f:(fun (ty, env_extension) -> Get_tag ty, env_extension)
    | Is_int ty, Naked_immediates is_int ->
      begin match I.Set.elements is_int with
      | [] -> Bottom
      | [is_int] ->
        let shape =
          if I.equal is_int I.zero then T.any_block ()
          else if I.equal is_int I.one then T.any_tagged_immediate ()
          else bad_meet_or_join env t1 t2
        in
        Or_bottom_or_absorbing.of_or_bottom
          (E.switch T.meet T.join' env ty shape)
          ~f:(fun (ty, env_extension) -> Is_int ty, env_extension)
      | n1 :: n2 :: [] ->
        (* Note: Set.elements returns a sorted list, so n1 = 1 && n2 = 0
           should never occur *)
        if I.equal n1 I.zero && I.equal n2 I.one
        then Ok (Is_int ty, TEE.empty ())
        else bad_meet_or_join env t1 t2
      | _::_::_::_ -> bad_meet_or_join env t1 t2
      end
    | Naked_immediates is_int, Is_int ty ->
      begin match I.Set.elements is_int with
      | [] -> Bottom
      | [is_int] ->
        let shape =
          if I.equal is_int I.zero then T.any_block ()
          else if I.equal is_int I.one then T.any_tagged_immediate ()
          else bad_meet_or_join env t1 t2
        in
        Or_bottom_or_absorbing.of_or_bottom
          (E.switch T.meet T.join' env shape ty)
          ~f:(fun (ty, env_extension) -> Is_int ty, env_extension)
      | n1 :: n2 :: [] ->
        if I.equal n1 I.zero && I.equal n2 I.one
        then Ok (Is_int ty, TEE.empty ())
        else bad_meet_or_join env t1 t2
      | _::_::_::_ -> bad_meet_or_join env t1 t2
      end
    | Get_tag ty, Naked_immediates tags ->
      (* CR mshinwell: eliminate code duplication, same above.  Or-patterns
         aren't the answer, since join depends on the left/right envs! *)
      let tags =
        Target_imm.Set.fold (fun tag tags ->
            match Target_imm.to_tag tag with
            | Some tag -> Tag.Set.add tag tags
            | None -> bad_meet_or_join env t1 t2)
          tags
          Tag.Set.empty
      in
      begin match T.blocks_with_these_tags tags with
      | Known shape ->
        Or_bottom_or_absorbing.of_or_bottom
          (E.switch T.meet T.join' env ty shape)
          ~f:(fun (ty, env_extension) -> Get_tag ty, env_extension)
      | Unknown ->
        begin match E.op () with
        | Meet -> Ok (Get_tag ty, TEE.empty ())
        | Join ->
          (* This should be Top, but we only have Absorbing; fortunately,
             since we're in the join case, Absorbing turns out to be Top *)
          Absorbing
        end
      end
    | (Is_int _ | Get_tag _), (Is_int _ | Get_tag _) -> Absorbing
    | Naked_immediates tags, Get_tag ty ->
      let tags =
        Target_imm.Set.fold (fun tag tags ->
            match Target_imm.to_tag tag with
            | Some tag -> Tag.Set.add tag tags
            | None -> bad_meet_or_join env t1 t2)
          tags
          Tag.Set.empty
      in
      begin match T.blocks_with_these_tags tags with
      | Known shape ->
        Or_bottom_or_absorbing.of_or_bottom
          (E.switch T.meet T.join' env ty shape)
          ~f:(fun (ty, env_extension) -> Get_tag ty, env_extension)
      | Unknown ->
        begin match E.op () with
        | Meet -> Ok (Get_tag ty, TEE.empty ())
        | Join ->
          (* This should be Top, but we only have Absorbing; fortunately,
             since we're in the join case, Absorbing turns out to be Top *)
          Absorbing
        end
      end
end
