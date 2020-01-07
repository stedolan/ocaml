(* TEST
   flags = "-g"
   compare_programs = "false" *)

open Gc.Memprof

let f4 n = (n,n,n,n)

let[@inline never] f n =
  (n, (n, n, f4 n))

let test sampling_rate =
  let counts = Array.make 257 0 in
  start ~callstack_size:10
    ~minor_alloc_callback:(fun info ->
      counts.(info.size) <- counts.(info.size) + info.n_samples; Some info.size)
    ~minor_dealloc_callback:(fun sz ->
      assert (sz = 2 || sz = 3))
    ~sampling_rate ();
  let iter = 100_000 in
  let arr = Array.init iter (fun i ->
    let (_, (_, _, x)) = Sys.opaque_identity f i in
    x) in
  stop ();
  ignore (Sys.opaque_identity arr);
  for i = 0 to 256 do
    if counts.(i) > 0 then begin
      let total = (i + 1) * iter in
      Printf.printf "%d: %.2f\n" i
        (float_of_int counts.(i) /. float_of_int total)
    end
  done

let () =
  List.iter test [0.42; 0.01; 0.83]
