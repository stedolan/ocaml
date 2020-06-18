(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = Flambda_kind.t list

let nullary = []

let create t = t

let length t = List.length t

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 = Misc.Stdlib.List.compare Flambda_kind.compare t1 t2

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash = Hashtbl.hash

  let print ppf t =
    match t with
    | [] -> Format.pp_print_string ppf "Nullary"
    | _ ->
      Format.fprintf ppf "@[%a@]"
        (Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf " @<1>\u{2a2f} ")
          Flambda_kind.print)
        t

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

let is_all_values t =
  List.for_all Flambda_kind.is_value t

let is_all_naked_floats t =
  List.for_all Flambda_kind.is_naked_float t

let is_singleton_value t =
  match t with
  | [kind] when Flambda_kind.equal kind Flambda_kind.value -> true
  | _ -> false
