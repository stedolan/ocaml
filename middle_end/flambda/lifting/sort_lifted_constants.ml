(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

module SCC_lifted_constants = Strongly_connected_components.Make (CIS)

type result = {
  bindings_outermost_last : LC.t list;
}

let empty_result = {
  bindings_outermost_last = [];
}

let build_dep_graph ~fold_over_lifted_constants =
  (* Format.eprintf "SORTING:\n%!"; *)
  fold_over_lifted_constants ~init:(CIS.Map.empty, CIS.Map.empty)
    ~f:(fun (dep_graph, code_id_or_symbol_to_const) lifted_constant ->
      (* Format.eprintf "One constant: %a\n%!" LC.print lifted_constant; *)
      ListLabels.fold_left (LC.definitions lifted_constant)
        ~init:(dep_graph, code_id_or_symbol_to_const)
        ~f:(fun (dep_graph, code_id_or_symbol_to_const) definition ->
          let module D = LC.Definition in
          let free_names =
            let free_names =
              Static_const.free_names (D.defining_expr definition)
            in
            match D.descr definition with
            | Code _ | Block_like _ -> free_names
            | Set_of_closures { denv = _; closure_symbols_with_types; } ->
              (* To avoid existing sets of closures (with or without associated
                 code) being pulled apart, we add a dependency from each of the
                 closure symbols (in the current set) to all of the others
                 (in the current set). *)
              ListLabels.fold_left
                (Closure_id.Lmap.data closure_symbols_with_types)
                ~init:free_names
                ~f:(fun free_names (symbol, _) ->
                  Name_occurrences.add_symbol free_names symbol NM.normal)
          in
          (* CR mshinwell: Maybe there's some other way of dealing with
             the insertion of the constants for [Reify_continuation_params]
             that wouldn't involve adding "let symbol" bindings before
             simplification (of those bindings around the handler).  If we
             could do that, the following code could be removed. *)
          (* Beware: when coming from [Reify_continuation_params] the
             sets of closures may have dependencies on variables that are
             now equal to symbols in the environment.  (They haven't been
             changed to symbols yet as the simplifier hasn't been run on
             the definitions.)  Some of these symbols may be the ones
             involved in the current SCC calculation.  For this latter set,
             we must explicitly add them as dependencies. *)
          let free_syms = Name_occurrences.symbols free_names in
          (* RCP is disabled at present
            Name_occurrences.fold_names free_names
              ~init:Symbol.Set.empty
              ~f:(fun free_syms name ->
                Name.pattern_match name
                  ~var:(fun var ->
                    try
                      let typing_env =
                        match D.denv definition with
                        | Some denv -> DE.typing_env denv
                        | None -> assert false  (* see above *)
                      in
                      match
                        TE.get_canonical_simple_exn typing_env
                          ~min_name_mode:NM.normal
                          (Simple.var var)
                      with
                      | exception Not_found -> free_syms
                      | canonical ->
                        Simple.pattern_match canonical
                          ~const:(fun _ -> free_syms)
                          ~name:(fun name ->
                            Name.pattern_match name
                              ~var:(fun _var -> free_syms)
                              ~symbol:(fun sym -> Symbol.Set.add sym free_syms))
                    with Misc.Fatal_error -> begin
                      if !Clflags.flambda_context_on_error then begin
                        Format.eprintf "\n%sContext is:%s finding canonical \
                            for %a,@ current constant is:@ %a@ \
                            with [free_names]:@ %a"
                          (Flambda_colours.error ())
                          (Flambda_colours.normal ())
                          Variable.print var
                          LC.print lifted_constant
                          Name_occurrences.print free_names
                      end;
                      raise Misc.Fatal_error
                    end)
                  ~symbol:(fun sym -> Symbol.Set.add sym free_syms))
          in
          *)
          let free_code_ids =
            free_names
            |> Name_occurrences.code_ids_and_newer_version_of_code_ids
          in
          let deps =
            CIS.Set.union (CIS.set_of_symbol_set free_syms)
              (CIS.set_of_code_id_set free_code_ids)
          in
          let being_defined =
            D.bound_symbols definition
            |> Bound_symbols.everything_being_defined
          in
          CIS.Set.fold
            (fun being_defined (dep_graph, code_id_or_symbol_to_const) ->
              let dep_graph = CIS.Map.add being_defined deps dep_graph in
              let code_id_or_symbol_to_const =
                CIS.Map.add being_defined lifted_constant
                  code_id_or_symbol_to_const
              in
              dep_graph, code_id_or_symbol_to_const)
            being_defined
            (dep_graph, code_id_or_symbol_to_const)))

let sort ~fold_over_lifted_constants =
  (* The various lifted constants may exhibit recursion between themselves
     (specifically between closures and/or code).  We use SCC to obtain a
     topological sort of groups that must be coalesced into single
     code-and-set-of-closures definitions. *)
  let lifted_constants_dep_graph, code_id_or_symbol_to_const =
    build_dep_graph ~fold_over_lifted_constants
  in
  (*
  Format.eprintf "SCC graph is:@ %a\n%!"
    (CIS.Map.print CIS.Set.print)
    lifted_constants_dep_graph;
  *)
  let bindings_outermost_last =
    lifted_constants_dep_graph
    |> SCC_lifted_constants.connected_components_sorted_from_roots_to_leaf
    |> Array.to_list
    |> List.rev
    |> ListLabels.fold_left ~init:[]
         ~f:(fun bindings (group : SCC_lifted_constants.component) ->
           let code_id_or_symbols =
             match group with
             | No_loop code_id_or_symbol -> [code_id_or_symbol]
             | Has_loop code_id_or_symbols -> code_id_or_symbols
           in
           let _, lifted_constants =
             ListLabels.fold_left code_id_or_symbols
               ~init:(CIS.Set.empty, [])
               ~f:(fun ((already_seen, definitions) as acc) code_id_or_symbol ->
                 if CIS.Set.mem code_id_or_symbol already_seen then acc
                 else
                   let lifted_constant =
                     CIS.Map.find code_id_or_symbol code_id_or_symbol_to_const
                   in
                   let already_seen =
                     (* We may encounter the same defining expression more
                        than once, in the case of sets of closures, which
                        may bind more than one symbol.  We must avoid
                        duplicates in the result list. *)
                     let bound_symbols = LC.bound_symbols lifted_constant in
                     CIS.Set.union
                       (Bound_symbols.everything_being_defined bound_symbols)
                       already_seen
                   in
                   already_seen, lifted_constant :: definitions)
           in
           (LC.concat lifted_constants) :: bindings)
  in
  (* By reversing the list upon a subsequent fold we rely on the following
     property:
       Let the list L be a topological sort of a directed graph G.
       Then the reverse of L is a topological sort of the transpose of G.
  *)
  (*
  Format.eprintf "Result, outermost first:@ %a\n%!"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space LC.print)
    (List.rev bindings_outermost_last);
  Format.eprintf "Result, outermost first:@ %a\n%!"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Bound_symbols.print)
    (List.map LC.bound_symbols (List.rev bindings_outermost_last));
  *)
  { bindings_outermost_last; }
