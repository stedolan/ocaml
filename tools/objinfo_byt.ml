open Printf

let print_cmx_approx (ui : Cmx_format.unit_infos) =
  match ui.ui_export_info with
  | Clambda _ ->
     printf "Clambda unit\n"
  | Flambda _ ->
     printf "Flambda unit\n"

let _ = Objinfo.main ~print_cmx_approx ()
