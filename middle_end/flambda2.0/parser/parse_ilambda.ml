module Lex = Flambda_lex
module Parser = Flambda_parser

type error =
  | Lexing_error of Lex.error * Lambda.scoped_location
  | Parsing_error of Lambda.scoped_location

let make_loc (startpos, endpos) = {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let parse_program filename =
  let ic = open_in filename in
  try
    let pos = { Lexing.pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
    let lb = Lexing.from_channel ic in
    let lb = { lb with lex_start_p = pos; lex_curr_p = pos } in
    let program =
      try Ok (Parser.program Lex.token lb)
      with
      | Parser.Error ->
        let loc = make_loc (Lexing.lexeme_start_p lb, Lexing.lexeme_end_p lb) in
        Error (Parsing_error (Lambda.of_raw_location loc))
      | Lex.Error (error, loc) ->
        Error (Lexing_error (error, Lambda.of_raw_location (make_loc loc)))
    in
    close_in ic;
    program
  with
  | e ->
    let x = Printexc.get_raw_backtrace () in
    close_in ic;
    Printexc.raise_with_backtrace e x

let check_invariants program =
  try Flambda_unit.invariant program
  with exn ->
    Format.eprintf "Program which failed invariant check:@ %a\n%!"
      Flambda_unit.print program;
    raise exn

let parse_ilambda ~backend file =
    match parse_program file with
    | Ok program ->
      Format.printf "%a@.@."
        Print_fexpr.program program;
      let il = Fexpr_to_ilambda.conv program in
      Format.printf "ilambda:@.%a@.@."
        Ilambda.print_program il.program;
      let fl2 =
        Closure_conversion.ilambda_to_flambda ~backend
          ~module_ident:il.module_ident
          ~module_block_size_in_words:il.size
          ~filename:"/tmp/toto"
          il.program
      in
      Format.printf "flambda:@.%a@.@."
        Flambda_unit.print fl2;
      check_invariants fl2;
      let fl2' = Simplify.run ~backend ~round:1 fl2 in
      Format.printf "simplify:@.%a@."
        Flambda_unit.print fl2'.unit;
      fl2'
    | Error e ->
      begin match e with
      | Parsing_error loc ->
        Format.eprintf
          "%a:@.\
           Syntax error@."
          Lambda.print_scoped_location loc
      | Lexing_error (error, loc) ->
        Format.eprintf
          "%a:@.\
           Lex error: %a@."
          Lambda.print_scoped_location loc
          Lex.pp_error error
      end;
      exit 1

let parse_flambda ~backend file =
    match parse_program file with
    | Ok program ->
      Format.printf "%a@.@."
        Print_fexpr.program program;
      let fl2 = Fexpr_to_flambda.conv ~backend program in
      Format.printf "flambda:@.%a@.@."
        Flambda_unit.print fl2;
      check_invariants fl2;
      let fl2' = Simplify.run ~backend ~round:1 fl2 in
      Format.printf "simplify:@.%a@."
        Flambda_unit.print fl2'.unit;
      fl2'
    | Error e ->
      begin match e with
      | Parsing_error loc ->
        Format.eprintf
          "%a:@.\
           Syntax error@."
          Lambda.print_scoped_location loc
      | Lexing_error (error, loc) ->
        Format.eprintf
          "%a:@.\
           Lex error: %a@."
          Lambda.print_scoped_location loc
          Lex.pp_error error
      end;
      exit 1

let go ~backend () =
  let file = Sys.argv.(1) in
  let ext = Filename.extension file in
  match ext with
  | ".il" -> parse_ilambda ~backend file
  | ".fl" -> parse_flambda ~backend file
  | _ ->
    Misc.fatal_errorf "Unrecognized extension %s" ext
