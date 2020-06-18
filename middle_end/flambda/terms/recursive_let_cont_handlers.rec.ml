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
      handlers : Continuation_handlers.t;
      body : Expr.t;
    }

  let invariant _env _t = ()

  let print _ppf _t = Misc.fatal_error "Not yet implemented"
  let print_with_cache ~cache:_ _ppf _t = Misc.fatal_error "Not yet implemented"

  let create ~body handlers =
    { handlers;
      body;
    }

  let handlers t = t.handlers
  let body t = t.body

  let free_names { handlers; body; } =
    Name_occurrences.union (Continuation_handlers.free_names handlers)
      (Expr.free_names body)

  let apply_name_permutation { handlers; body; } perm =
    let handlers' =
      Continuation_handlers.apply_name_permutation handlers perm
    in
    let body' =
      Expr.apply_name_permutation body perm
    in
    { handlers = handlers';
      body = body';
    }

  let all_ids_for_export { handlers; body; } =
    let body_ids = Expr.all_ids_for_export body in
    let handlers_ids = Continuation_handlers.all_ids_for_export handlers in
    Ids_for_export.union body_ids handlers_ids

  let import import_map { handlers; body; } =
    let body = Expr.import import_map body in
    let handlers = Continuation_handlers.import import_map handlers in
    { handlers; body; }
end

include Name_abstraction.Make_list (Bindable_continuation) (T0)

let invariant _env _t = ()

let create ~body handlers =
  let bound = Continuation_handlers.domain handlers in
  let handlers0 = T0.create ~body handlers in
  create (Continuation.Set.elements bound) handlers0

let pattern_match t ~f =
  pattern_match t ~f:(fun _bound handlers0 ->
      let body = T0.body handlers0 in
      let handlers = T0.handlers handlers0 in
      f ~body handlers)
