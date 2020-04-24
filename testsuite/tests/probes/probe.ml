(* TEST
* native
** flags = "-probes"
** flags = "-no-probes"
* bytecode
*)

let () =
  [%probe "boo" (Printf.printf "BOO!\n")];
  let boo_enabled = [%probe_is_enabled "boo"] in
  assert (not boo_enabled)

let f x y z =
  Printf.printf "f: %d %f %s\n" x y z

let foo a b c =
  [%probe "fooia" (f a b c)];
  Printf.printf "from foo %d %f\n" a b

let rec fib i j =
  if (i >= 0) then begin
    if (i mode 2 = 0) then
      foo (i+j) Float.((of_int i) /. (of_int j)) "myau";
    fib j (i+j)
  end

let () =
  [%probe "boo" (Printf.printf "BOO!\n")];
  fib 0 1;
  let boo_enabled = [%probe_is_enabled "boo"] in
  assert (not boo_enabled)
