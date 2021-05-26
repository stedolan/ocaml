open Printf

let print_cmx_approx (ui : Cmx_format.unit_infos) =
  match ui.ui_export_info with
  | Clambda approx ->
    if not !Objinfo.no_approx then begin
      printf "Clambda approximation:\n";
      Format.fprintf Format.std_formatter "  %a@." Printclambda.approx approx
    end else
      Format.printf "Clambda unit@.";
  | Flambda export ->
    if not !Objinfo.no_approx || not !Objinfo.no_code then
      printf "Flambda export information:\n"
    else
      printf "Flambda unit\n";
    if not !Objinfo.no_approx then begin
      let cu =
        Compilation_unit.create (Ident.create_persistent ui.ui_name)
          (Linkage_name.create "__dummy__")
      in
      Compilation_unit.set_current cu;
      let root_symbols =
        List.map (fun s ->
            Symbol.of_global_linkage cu (Linkage_name.create ("caml"^s)))
          ui.ui_defines
      in
      Format.printf "approximations@ %a@.@."
        Export_info.print_approx (export, root_symbols)
    end;
    if not !Objinfo.no_code then
      Format.printf "functions@ %a@.@."
        Export_info.print_functions export

let _ = Objinfo.main ~print_cmx_approx ()
