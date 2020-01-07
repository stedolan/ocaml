(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2020 OCamlPro SAS                                    *)
(*   Copyright 2018--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t = {
  param : Variable.t;
  kind : Flambda_kind.t;
}

include Identifiable.Make (struct
  type nonrec t = t

  let compare 
        { param = param1; kind = kind1; }
        { param = param2; kind = kind2; } =
    let c = Variable.compare param1 param2 in
    if c <> 0 then c
    else Flambda_kind.compare kind1 kind2

  let equal t1 t2 = compare t1 t2 = 0

  let hash { param; kind; } =
    Hashtbl.hash (Variable.hash param, Flambda_kind.hash kind)

  let print ppf { param; kind; } =
    Format.fprintf ppf "@[(@<0>%s%a@<0>%s @<1>\u{2237} %a)@]"
      (Flambda_colours.parameter ())
      Variable.print param
      (Flambda_colours.normal ())
      Flambda_kind.print kind

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

let print_with_cache ~cache:_ ppf t = print ppf t

let create param kind =
  { param;
    kind;
  }

let var t = t.param
let name t = Name.var (var t)
let simple t = Simple.var (var t)
let kind t = t.kind

let with_kind t kind = { t with kind; }

let rename t = { t with param = Variable.rename t.param; }

let map_var t ~f = { t with param = f t.param; }

let map_kind t ~f = { t with kind = f t.kind; }

let equal_kinds t1 t2 =
  Flambda_kind.equal t1.kind t2.kind

let free_names ({ param = _; kind = _; } as t) =
  Name_occurrences.singleton_variable (var t) Name_mode.normal

let apply_name_permutation ({ param = _; kind; } as t) perm =
  Name.pattern_match (Name_permutation.apply_name perm (name t))
    ~var:(fun var -> create var kind)
    ~symbol:(fun _ ->
      Misc.fatal_errorf "Illegal name permutation on [Kinded_parameter]: %a"
        Name_permutation.print perm)

let all_ids_for_export { param; kind = _; } =
  Ids_for_export.add_variable Ids_for_export.empty param

let import import_map { param; kind; } =
  let param = Ids_for_export.Import_map.variable import_map param in
  { param; kind; }

let add_to_name_permutation t ~guaranteed_fresh perm =
  Name_permutation.add_fresh_variable perm (var t)
    ~guaranteed_fresh:(var guaranteed_fresh)

let name_permutation t ~guaranteed_fresh =
  add_to_name_permutation t ~guaranteed_fresh Name_permutation.empty

let singleton_occurrence_in_terms t =
  Name_occurrences.singleton_variable (var t) Name_mode.normal

let add_occurrence_in_terms t occs =
  Name_occurrences.add_variable occs (var t) Name_mode.normal

module List = struct
  type nonrec t = t list

  let create params_and_kinds =
    List.map (fun (param, kind) -> create param kind) params_and_kinds

  let vars t = List.map var t

  let simples t = List.map simple t

  let equal_vars t1 t2 =
    List.length t1 = List.length t2
      && List.for_all2 (fun param1 var2 -> Variable.equal (var param1) var2)
           t1 t2

  let var_set t = Variable.Set.of_list (vars t)

  let name_set t = Name.Set.of_list (List.map Name.var (vars t))

  let simple_set t = Simple.Set.of_list (simples t)

  let rename t = List.map (fun t -> rename t) t

  let arity t = List.map (fun t -> kind t) t

  let equal t1 t2 =
    List.compare_lengths t1 t2 = 0
      && List.for_all2 equal t1 t2

  let print ppf t =
    Format.fprintf ppf "@[<hov 0>%a@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space print) t

  let free_names t =
    List.fold_left (fun result param ->
        Name_occurrences.union result (free_names param))
      (Name_occurrences.empty)
      t

  let apply_name_permutation t perm =
    List.map (fun param -> apply_name_permutation param perm) t
end
