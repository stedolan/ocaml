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

let t6 x =
  [%probe "handler_not_unit" (x+1)]

let () =  t6 6

