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

module Int32 = Numbers.Int32
module TEE = Typing_env_extension

type t = Int32.Set.t

let print ppf t =
  Format.fprintf ppf "@[(Naked_int32s@ (%a))@]" Int32.Set.print t

let print_with_cache ~cache:_ ppf t = print ppf t

let apply_name_permutation t _perm = t

let free_names _t = Name_occurrences.empty

let all_ids_for_export _t = Ids_for_export.empty

let import _import_map t = t

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
  let meet_or_join _env t1 t2 : _ Or_bottom_or_absorbing.t =
    let t = E.Int32.Set.union_or_inter t1 t2 in
    if Int32.Set.is_empty t then Bottom
    else Ok (t, TEE.empty ())
end
