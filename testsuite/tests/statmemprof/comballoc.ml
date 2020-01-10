(* TEST
   flags = "-g"
   compare_programs = "false" *)

open Gc.Memprof

let f4 n = (n,n,n,n)

let[@inline never] f n =
  (n, (n, n, f4 n))

let test sampling_rate =
  let allocs = Array.make 257 0 in
  let deallocs = Array.make 257 0 in
  let promotes = Array.make 257 0 in
  start ~callstack_size:10
    ~minor_alloc_callback:(fun info ->
      allocs.(info.size) <- allocs.(info.size) + info.n_samples;
      Some (info.size, info.n_samples))
    ~minor_dealloc_callback:(fun (sz,n) ->
      deallocs.(sz) <- deallocs.(sz) + n)
    ~promote_callback:(fun (sz,n) ->
      promotes.(sz) <- promotes.(sz) + n;
      None)
    ~sampling_rate ();
  let iter = 100_000 in
  let arr = Array.init iter (fun i ->
    let (_, (_, _, x)) = Sys.opaque_identity f i in
    x) in
  Gc.minor ();
  stop ();
  ignore (Sys.opaque_identity arr);
  for i = 0 to 256 do
    assert (deallocs.(i) + promotes.(i) = allocs.(i));
    if allocs.(i) > 0 then begin
      let total = (i + 1) * iter in
      (* allocs.(i) / total is
           Binomial(total, rate) / total
         which is approx.
           Normal(total * rate, total * rate*(1-rate)) / total
         which is
           Normal(1, rate*(1-rate) / total)
         which has stddev sqrt(rate*(1-rate)/total)
         which is less than 10^-3 for the values here.
         So, an error of 0.005 (enough to make %.2f print differently)
         is a 5-sigma event, with probability less than 3*10^-7 *)
      Printf.printf "%d: %.2f %b\n" i
        (float_of_int allocs.(i) /. float_of_int total)
        (promotes.(i) > 1000)
    end
  done

let () =
  List.iter test [0.42; 0.01; 0.83]


let no_callback_after_stop trigger =
  let stopped = ref false in
  let cnt = ref 0 in
  start ~callstack_size:0
    ~minor_alloc_callback:(fun info ->
      assert(not !stopped);
      incr cnt;
      if !cnt > trigger then begin
        start ~callstack_size:0
          ~minor_alloc_callback:(fun _ -> assert false)
          ~sampling_rate:0. ();
        stopped := true
      end;
      None
    )
    ~sampling_rate:1. ();
  for i = 0 to 1000 do ignore (Sys.opaque_identity f i) done;
  assert !stopped

let () =
  for i = 0 to 1000 do no_callback_after_stop i done;
  Printf.printf "OK\n"
