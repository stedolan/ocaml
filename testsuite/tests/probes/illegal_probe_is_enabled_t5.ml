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

let f x =
  Printf.printf "%d\n" x

let t5 x =
  if [%probe_is_enabled "same rules here for probe names"] then
    f x
  else
    f (-x)

let () = t5 5


