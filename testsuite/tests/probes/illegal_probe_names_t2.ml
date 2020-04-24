(* TEST

 * setup-ocamlc.byte-build-env
** ocamlc.byte
ocamlc_byte_exit_status = "2"
*** check-ocamlc.byte-output

* setup-ocamlopt.byte-build-env
** ocamlopt.byte
ocamlopt_byte_exit_status = "2"
*** check-ocamlopt.byte-output
*)

let () =
  [%probe "this_probe_name_is_way_too_long_0123456789_0123456789_0123456789_0123456789_0123456789_0123456789_0123456789_0123456789_0123456789_0123456789_" ()]
