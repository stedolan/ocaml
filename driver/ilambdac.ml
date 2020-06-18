(* CR mshinwell: This probably needs some updating *)

module Flambda_backend = struct
  let symbol_for_global' id =
    Symbol.unsafe_create
      (Compilation_unit.get_current_exn ())
      (Linkage_name.create (Compilenv.symbol_for_global id))

  let division_by_zero =
    symbol_for_global' Predef.ident_division_by_zero

  let invalid_argument =
    symbol_for_global' Predef.ident_invalid_argument

  let closure_symbol _ = failwith "Not yet implemented"
  let really_import_approx _ = failwith "Not yet implemented"
  let import_symbol _ = failwith "Not yet implemented"

  let all_predefined_exception_symbols = Symbol.Set.empty (* XXX *)

  let size_int = Arch.size_int
  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    Proc.max_arguments_for_tailcalls - 1
end

let flambda_backend = (module Flambda_backend : Flambda_backend_intf.S)

let () =
  Clflags.dump_cmm := true;
  Clflags.keep_asm_file := true;
  let fl = Parse_ilambda.go ~backend:flambda_backend () in
  Asmgen.compile_implementation_flambda_for_ilambdac
    ?toplevel:None
    ~prefixname:"test"
    ~ppf_dump:Format.std_formatter
    ~required_globals:Ident.Set.empty
    fl
