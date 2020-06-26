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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

let rec load_cmx_file_contents backend comp_unit ~imported_units ~imported_names
      ~imported_code =
  match Compilation_unit.Map.find comp_unit !imported_units with
  | typing_env_or_none -> typing_env_or_none
  | exception Not_found ->
    let module Backend = (val backend : Flambda_backend_intf.S) in
    match Backend.get_global_info comp_unit with
    | None ->
      (* To make things easier to think about, we never retry after a .cmx
         load fails. *)
      imported_units := Compilation_unit.Map.add comp_unit None !imported_units;
      None
    | Some cmx ->
      let resolver comp_unit =
        load_cmx_file_contents backend comp_unit ~imported_names ~imported_code
          ~imported_units
      in
      let get_imported_names () = !imported_names in
      let typing_env, code =
        Flambda_cmx_format.import_typing_env_and_code cmx
      in
      let typing_env =
        TE.Serializable.to_typing_env ~resolver ~get_imported_names typing_env
      in
      let newly_imported_names = TE.name_domain typing_env in
      imported_names := Name.Set.union newly_imported_names !imported_names;
      imported_code := Exported_code.merge code !imported_code;
      let offsets = Flambda_cmx_format.exported_offsets cmx in
      Exported_offsets.import_offsets offsets;
      imported_units :=
        Compilation_unit.Map.add comp_unit (Some typing_env) !imported_units;
      Some typing_env

let prepare_cmx_file_contents ~return_cont_env:cont_uses_env
      ~return_continuation ~module_symbol all_code =
  match
    Continuation_uses_env.get_typing_env_no_more_than_one_use
      cont_uses_env return_continuation
  with
  | None -> None
  | Some _ when !Clflags.opaque -> None
  | Some final_typing_env ->
    (* CR mshinwell: We should remove typing information about names that
       do not occur (transitively) in the type of the module block. *)
    let final_typing_env =
      TE.clean_for_export final_typing_env ~module_symbol
      |> TE.Serializable.create
    in
    let exported_offsets =
      (* The offsets computations for newly defined elements will be added
         after Un_cps_closure *)
      Exported_offsets.imported_offsets ()
    in
    Some (Flambda_cmx_format.create
            ~final_typing_env ~all_code ~exported_offsets)
