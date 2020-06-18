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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module T0 = struct
  type t = {
    handler : Expr.t;
  }

  let print_with_cache ~cache ppf { handler; } =
    fprintf ppf "@[<hov 1>(\
        @[<hov 1>(handler@ %a)@]\
        )@]"
      (Expr.print_with_cache ~cache) handler

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let free_names { handler; } =
    Expr.free_names handler

  let apply_name_permutation ({ handler; } as t) perm =
    let handler' =
      Expr.apply_name_permutation handler perm
    in
    if handler == handler' then t
    else { handler = handler'; }

  let all_ids_for_export { handler; } =
    Expr.all_ids_for_export handler

  let import import_map { handler; } =
    let handler = Expr.import import_map handler in
    { handler; }
end

include Name_abstraction.Make_list (Kinded_parameter) (T0)

let invariant _env _t = ()

let print ppf t : unit = print ppf t

let print_with_cache ~cache ppf t : unit = print_with_cache ~cache ppf t

let create params ~handler =
  let t0 : T0.t =
    { handler;
    }
  in
  create params t0

let pattern_match t ~f =
  pattern_match t ~f:(fun params { handler; } ->
    f params ~handler)

let pattern_match_pair t1 t2 ~f =
  pattern_match_pair t1 t2 ~f:(
    fun params { handler = handler1; } { handler = handler2; } ->
      f params ~handler1 ~handler2)
