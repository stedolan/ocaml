(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda

type t = {
  definitions :
    (Continuation_handler.t * Name_occurrences.t) Continuation.Map.t;
  rev_deps_of_variables : Continuation.Set.t Variable.Map.t;
  rev_deps_of_continuations : Continuation.Set.t Continuation.Map.t;
}

let empty = {
  definitions = Continuation.Map.empty;
  rev_deps_of_variables = Variable.Map.empty;
  rev_deps_of_continuations = Continuation.Map.empty;
}

let add_handler t cont handler =
  let definitions =
    Continuation.Map.add cont
      (handler, Continuation_handler.free_names handler)
      t.definitions
  in
  let free_names = Continuation_handler.free_names handler in
  let rev_deps_of_variables =
    Name_occurrences.fold_variables free_names
      ~init:t.rev_deps_of_variables
      ~f:(fun rev_deps_of_variables var ->
        let conts =
          match Variable.Map.find var rev_deps_of_variables with
          | exception Not_found -> Continuation.Set.empty
          | conts -> conts
        in
        Variable.Map.add var (Continuation.Set.add cont conts)
          rev_deps_of_variables)
  in
  let rev_deps_of_continuations =
    Name_occurrences.fold_continuations_including_in_trap_actions free_names
      ~init:t.rev_deps_of_continuations
      ~f:(fun rev_deps_of_continuations cont' ->
        let conts =
          match Continuation.Map.find cont' rev_deps_of_continuations with
          | exception Not_found -> Continuation.Set.empty
          | conts -> conts
        in
        Continuation.Map.add cont' (Continuation.Set.add cont conts)
          rev_deps_of_continuations)
  in
  { definitions;
    rev_deps_of_variables;
    rev_deps_of_continuations;
  }

let rec transitive_closure0 t conts ~result =
  match Continuation.Set.choose conts with
  | exception Not_found -> result
  | cont ->
    let result = Continuation.Set.add cont result in
    let conts = Continuation.Set.remove cont conts in
    let rev_deps =
      match Continuation.Map.find cont t.rev_deps_of_continuations with
      | exception Not_found -> Continuation.Set.empty
      | rev_deps_of_cont -> rev_deps_of_cont
    in
    let to_traverse = Continuation.Set.diff rev_deps result in
    transitive_closure0 t (Continuation.Set.union conts to_traverse) ~result

let transitive_closure t conts =
  transitive_closure0 t conts ~result:Continuation.Set.empty 

let find_rev_deps t names =
  let rev_deps =
    Name_occurrences.fold_variables names
      ~init:Continuation.Set.empty
      ~f:(fun rev_deps var ->
        let rev_deps_of_var =
          match Variable.Map.find var t.rev_deps_of_variables with
          | exception Not_found -> Continuation.Set.empty
          | rev_deps_of_var -> rev_deps_of_var
        in
        Continuation.Set.union rev_deps_of_var rev_deps)
  in
  let rev_deps =
    Name_occurrences.fold_continuations_including_in_trap_actions names
      ~init:rev_deps
      ~f:(fun rev_deps cont ->
        let rev_deps_of_cont =
          match Continuation.Map.find cont t.rev_deps_of_continuations with
          | exception Not_found -> Continuation.Set.empty
          | rev_deps_of_cont -> rev_deps_of_cont
        in
        Continuation.Set.union rev_deps_of_cont rev_deps)
  in
  let rev_deps = transitive_closure t rev_deps in
  Continuation.Set.fold (fun cont result ->
      match Continuation.Map.find cont t.definitions with
      | exception Not_found ->
        (* The continuation may not have an explicit handler (e.g. a
           return continuation. *)
        result
      | handler -> Continuation.Map.add cont handler result)
    rev_deps
    Continuation.Map.empty

let all t = t.definitions

let remove_domain_of_map t map =
  let definitions =
    Continuation.Map.filter (fun cont _ -> not (Continuation.Map.mem cont map))
      t.definitions
  in
  { definitions;
    rev_deps_of_variables = t.rev_deps_of_variables;
    rev_deps_of_continuations = t.rev_deps_of_continuations;
  }

let union
      { definitions = definitions1;
        rev_deps_of_variables = rev_deps_of_variables1;
        rev_deps_of_continuations = rev_deps_of_continuations1;
      }
      { definitions = definitions2;
        rev_deps_of_variables = rev_deps_of_variables2;
        rev_deps_of_continuations = rev_deps_of_continuations2;
      } =
  let definitions =
    Continuation.Map.disjoint_union definitions1 definitions2
  in
  let rev_deps_of_variables =
    Variable.Map.union
      (fun _ conts1 conts2 -> Some (Continuation.Set.union conts1 conts2))
      rev_deps_of_variables1
      rev_deps_of_variables2
  in
  let rev_deps_of_continuations =
    Continuation.Map.union
      (fun _ conts1 conts2 -> Some (Continuation.Set.union conts1 conts2))
      rev_deps_of_continuations1
      rev_deps_of_continuations2
  in
  { definitions;
    rev_deps_of_variables;
    rev_deps_of_continuations;
  }
