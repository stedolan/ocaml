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

type middle_end_result = {
  (* CR mshinwell: This next field is redundant *)
  cmx : Flambda_cmx_format.t option;
  unit : Flambda_unit.t;
  all_code : Exported_code.t;
}

let check_invariants unit =
  try Flambda_unit.invariant unit
  with exn -> begin
    Format.eprintf "Unit which failed invariant check:@ %a\n%!"
      Flambda_unit.print unit;
    raise exn
  end

let print_prepared_lambda ppf lam =
  if !Clflags.dump_prepared_lambda then begin
    Format.fprintf ppf "%sAfter Prepare_lambda:%s@ %a@."
      (Flambda_colours.each_file ())
      (Flambda_colours.normal ())
      Printlambda.lambda lam
  end

let print_ilambda ppf (ilam : Ilambda.program) =
  if !Clflags.dump_ilambda then begin
    Format.fprintf ppf
      "\n%sAfter CPS conversion (return continuation %a) \
       (exception continuation %a):%s@ %a@."
      (Flambda_colours.each_file ())
      Continuation.print ilam.return_continuation
      Continuation.print ilam.exn_continuation.exn_handler
      (Flambda_colours.normal ())
      Ilambda.print ilam.expr
  end

let print_ilambda_after_mutable_variable_elimination ppf
      (ilam : Ilambda.program) =
  if !Clflags.dump_ilambda then begin
    Format.fprintf ppf
      "\n%sAfter mutable variable elimination (return continuation %a) \
       (exception continuation %a):%s@ %a@."
      (Flambda_colours.each_file ())
      Continuation.print ilam.return_continuation
      Continuation.print ilam.exn_continuation.exn_handler
      (Flambda_colours.normal ())
      Ilambda.print ilam.expr
  end

let print_rawflambda ppf unit =
  if !Clflags.dump_rawflambda then begin
    Format.fprintf ppf "\n%sAfter closure conversion:%s@ %a@."
      (Flambda_colours.each_file ())
      (Flambda_colours.normal ())
      Flambda_unit.print unit
  end

let print_flambda name ppf unit =
  if !Clflags.dump_flambda then begin
    Format.fprintf ppf "\n%sAfter %s:%s@ %a@."
      (Flambda_colours.each_file ())
      name
      (Flambda_colours.normal ())
      Flambda_unit.print unit
  end

let middle_end0 ppf ~prefixname:_ ~backend ~filename ~module_ident
      ~module_block_size_in_words ~module_initializer =
  Misc.Color.setup !Clflags.color;
  Profile.record_call "flambda.0" (fun () ->
    let prepared_lambda, recursive_static_catches =
      Profile.record_call "prepare_lambda" (fun () ->
        Prepare_lambda.run module_initializer)
    in
    print_prepared_lambda ppf prepared_lambda;
    let ilambda =
      Profile.record_call "cps_conversion" (fun () ->
        Cps_conversion.lambda_to_ilambda prepared_lambda
          ~recursive_static_catches)
    in
    print_ilambda ppf ilambda;
    let ilambda =
      if ilambda.uses_mutable_variables then begin
        let ilambda =
          Profile.record_call "eliminate_mutable_variables" (fun () ->
            Eliminate_mutable_vars.run ilambda)
        in
        print_ilambda_after_mutable_variable_elimination ppf ilambda;
        ilambda
      end else begin
        ilambda
      end
    in
    let flambda =
      Profile.record_call "closure_conversion" (fun () ->
        Closure_conversion.ilambda_to_flambda ~backend ~module_ident
          ~module_block_size_in_words ~filename ilambda)
    in
    print_rawflambda ppf flambda;
    check_invariants flambda;
    let flambda =
      Profile.record_call ~accumulate:true "simplify"
        (fun () -> Simplify.run ~backend ~round:1 flambda)
    in
    print_flambda "simplify" ppf flambda.unit;
    flambda)

let middle_end ~ppf_dump:ppf ~prefixname ~backend ~filename ~module_ident
      ~module_block_size_in_words ~module_initializer : middle_end_result =
  try
    let simplify_result =
      middle_end0 ppf ~prefixname ~backend ~filename ~module_ident
        ~module_block_size_in_words ~module_initializer
    in
    { cmx = simplify_result.cmx;
      unit = simplify_result.unit;
      all_code = simplify_result.all_code;
    }
  with Misc.Fatal_error -> begin
    Format.eprintf "\n%sOriginal backtrace is:%s\n%s\n"
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      (Printexc.raw_backtrace_to_string (Misc.fatal_error_callstack ()));
    raise Misc.Fatal_error
  end
