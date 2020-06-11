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
(* CR mshinwell: Fix warning 60 *)
[@@@ocaml.warning "-60"]

open! Simplify_import

(* -- module rec binding here -- *)

type simplify_result = {
  cmx : Flambda_cmx_format.t option;
  unit : Flambda_unit.t;
}

let predefined_exception_typing_env ~backend ~resolver ~get_imported_names =
  let module Backend = (val backend : Flambda2_backend_intf.S) in
  let comp_unit = Compilation_unit.get_current_exn () in
  Compilation_unit.set_current (Compilation_unit.predefined_exception ());
  let typing_env =
    Symbol.Set.fold (fun sym typing_env ->
        TE.add_definition typing_env
          (Name_in_binding_pos.symbol sym)
          K.value)
      Backend.all_predefined_exception_symbols
      (TE.create ~resolver ~get_imported_names)
  in
  Compilation_unit.set_current comp_unit;
  typing_env

let run ~backend ~round unit =
  let module Backend = (val backend : Flambda2_backend_intf.S) in
  let return_continuation = FU.return_continuation unit in
  let exn_continuation = FU.exn_continuation unit in
  let module_symbol = FU.module_symbol unit in
  let imported_names = ref Name.Set.empty in
  let imported_code = ref Code_id.Map.empty in
  let imported_units = ref Compilation_unit.Map.empty in
  let resolver comp_unit =
    Flambda_cmx.load_cmx_file_contents backend comp_unit ~imported_names
      ~imported_code ~imported_units
  in
  let get_imported_names () = !imported_names in
  let get_imported_code () = !imported_code in
  let predefined_exception_typing_env =
    predefined_exception_typing_env ~backend ~resolver ~get_imported_names
  in
  imported_units :=
    Compilation_unit.Map.add (Compilation_unit.predefined_exception ())
      (Some predefined_exception_typing_env)
      !imported_units;
  imported_names :=
    Name.Set.union !imported_names
      (TE.name_domain predefined_exception_typing_env);
  let denv =
    DE.create ~round
      ~backend
      ~resolver
      ~get_imported_names
      ~get_imported_code
      ~float_const_prop:!Clflags.float_const_prop
      ~unit_toplevel_exn_continuation:exn_continuation
  in
  let return_cont_scope = DE.get_continuation_scope_level denv in
  let denv = DE.increment_continuation_scope_level denv in
  let exn_cont_scope = DE.get_continuation_scope_level denv in
  let denv = DE.increment_continuation_scope_level denv in
  let r = R.create ~resolver ~get_imported_names in
  let dacc = DA.create denv Continuation_uses_env.empty r in
  let body, dacc, r =
    let exn_continuation =
      Exn_continuation.create ~exn_handler:exn_continuation ~extra_args:[]
    in
    Simplify_toplevel.simplify_toplevel dacc (FU.body unit) ~return_continuation
      ~return_arity:[K.value] exn_continuation ~return_cont_scope
      ~exn_cont_scope
  in
  let return_cont_env = DA.continuation_uses_env dacc in
  let cmx =
    Flambda_cmx.prepare_cmx_file_contents ~return_cont_env
      ~return_continuation ~module_symbol (R.all_code r)
  in
  let unit =
    FU.create ~return_continuation ~exn_continuation ~module_symbol ~body
  in
  { cmx;
    unit;
  }
