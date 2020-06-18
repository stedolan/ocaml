(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = int Continuation.Map.t

let empty = Continuation.Map.empty

let use t k =
  Continuation.Map.update k (function
      | None -> Some 1
      | Some n -> Some (n + 1))
    t

let use_list t ks =
  List.fold_left (fun t k -> use t k) t ks

let create_singleton k = use empty k

let create_list ks = use_list empty ks

let union_list ts =
  List.fold_left (fun result t ->
      Continuation.Map.union_merge (fun n1 n2 -> n1 + n2) t result)
    empty
    ts

let union t1 t2 = union_list [t1; t2]

let to_map t = t
