(* TEST
   flags = "-g"
   * bytecode
     reference = "${test_source_directory}/arrays_in_minor.byte.reference"
   * native
     reference = "${test_source_directory}/arrays_in_minor.opt.reference"
     compare_programs = "false"
*)

open Gc.Memprof

type 'a list2 =  (* A list type where [Cons] has tag 1 *)
  | Nil
  | Dummy of int
  | Cons of 'a * 'a list2

let root = ref Nil
let[@inline never] allocate_arrays lo hi cnt keep =
  assert (0 < lo && hi <= 250);  (* Fits in minor heap. *)
  for j = 0 to cnt-1 do
    for i = lo to hi do
      root := Cons (Array.make i 0, !root)
    done;
    if not keep then root := Nil
  done

let check_nosample () =
  Printf.printf "check_nosample\n%!";
  let cb _ =
    Printf.printf "Callback called with sampling_rate = 0\n";
    assert(false)
  in
  start ~callstack_size:10 ~minor_alloc_callback:cb ~major_alloc_callback:cb
        ~sampling_rate:0. ();
  allocate_arrays 1 250 100 false

let () = check_nosample ()

let check_counts_full_major force_promote =
  Printf.printf "check_counts_full_major\n%!";
  let nalloc_minor = ref 0 in
  let enable = ref true in
  let npromote = ref 0 in
  let ndealloc_minor = ref 0 in
  let ndealloc_major = ref 0 in
  start ~callstack_size:10
        ~minor_alloc_callback:(fun info ->
          assert (info.tag = 0 || info.tag = 1);
          if !enable then begin
              incr nalloc_minor;
              Some ()
          end else
            None)
        ~major_alloc_callback:(fun _ -> assert false)
        ~promote_callback:(fun _ ->
           incr npromote;
           Some ())
        ~minor_dealloc_callback:(fun _ -> incr ndealloc_minor)
        ~major_dealloc_callback:(fun _ -> incr ndealloc_major)
        ~sampling_rate:0.01 ();
  allocate_arrays 1 250 100 true;
  enable := false;
  assert (!ndealloc_minor = 0 && !ndealloc_major = 0);
  if force_promote then begin
    Gc.full_major ();
    assert (!ndealloc_minor = 0 && !ndealloc_major = 0 &&
            !npromote = !nalloc_minor);
    root := Nil;
    Gc.full_major ();
    assert (!ndealloc_minor = 0 && !ndealloc_major = !nalloc_minor);
  end else begin
    root := Nil;
    Gc.minor ();
    Gc.full_major ();
    Gc.full_major ();
    assert (!nalloc_minor = !ndealloc_minor + !npromote &&
            !ndealloc_major = !npromote)
  end;
  stop ()

let () =
  check_counts_full_major false;
  check_counts_full_major true

let check_no_nested () =
  Printf.printf "check_no_nested\n%!";
  let in_callback = ref false in
  let cb _ =
    assert (not !in_callback);
    in_callback := true;
    allocate_arrays 1 100 10 false;
    ignore (Array.to_list (Array.make 1000 0));
    in_callback := false;
    ()
  in
  let cb' _ = cb (); Some () in
  start ~callstack_size:10
        ~minor_alloc_callback:cb' ~major_alloc_callback:cb'
        ~promote_callback:cb' ~minor_dealloc_callback:cb
        ~major_dealloc_callback:cb
    (* FIXME: we should use 1. to make sure the block is sampled,
       but the runtime does an infinite loop in native mode in this
       case. This bug will go away when the sampling of natively
       allocated will be correctly implemented. *)
        ~sampling_rate:0.5 ();
  allocate_arrays 1 250 5 false;
  stop ()

let () = check_no_nested ()

let check_distrib lo hi cnt rate =
  Printf.printf "check_distrib %d %d %d %f\n%!" lo hi cnt rate;
  let smp = ref 0 in
  start ~callstack_size:10
        ~major_alloc_callback:(fun _ -> assert false)
        ~minor_alloc_callback:(fun info ->
          (* Exclude noise such as spurious closures and the root list. *)
           if info.tag = 0 then begin
             assert (info.size >= lo && info.size <= hi);
             assert (info.n_samples > 0);
             assert (not info.unmarshalled);
             smp := !smp + info.n_samples
           end;
           None
        )
        ~sampling_rate:rate ();
  allocate_arrays lo hi cnt false;
  stop ();

  (* The probability distribution of the number of samples follows a
     binomial distribution of parameters tot_alloc and rate. Given
     that tot_alloc*rate and tot_alloc*(1-rate) are large (i.e., >
     100), this distribution is approximately equal to a normal
     distribution. We compute a 1e-8 confidence interval for !smp
     using quantiles of the normal distribution, and check that we are
     in this confidence interval. *)
  let tot_alloc = cnt*(lo+hi+2)*(hi-lo+1)/2 in
  assert (float tot_alloc *. rate > 100. &&
          float tot_alloc *. (1. -. rate) > 100.);
  let mean = float tot_alloc *. rate in
  let stddev = sqrt (float tot_alloc *. rate *. (1. -. rate)) in
  (* This assertion has probability to fail close to 1e-8. *)
  assert (abs_float (mean -. float !smp) <= stddev *. 5.7)

let () =
  check_distrib 1 250 1000 0.00001;
  check_distrib 1 250 1000 0.0001;
  check_distrib 1 250 1000 0.01;
  check_distrib 1 250 1000 0.9;
  check_distrib 1 1   10000000 0.01;
  check_distrib 250 250 100000 0.1

(* FIXME : in bytecode mode, the function [caml_get_current_callstack_impl],
   which is supposed to capture the current call stack, does not have access
   to the current value of [pc]. Therefore, depending on how the C call is
   performed, we may miss the first call stack slot in the captured backtraces.
   This is the reason why the reference file is different in native and
   bytecode modes.

   Note that [Printexc.get_callstack] does not suffer from this problem, because
   this function is actually an automatically generated stub which performs th
   C call. This is because [Printexc.get_callstack] is not declared as external
   in the mli file. *)

let[@inline never] check_callstack () =
  Printf.printf "check_callstack\n%!";
  let callstack = ref None in
  start ~callstack_size:10
        ~minor_alloc_callback:(fun info ->
          if info.tag = 0 then callstack := Some info.callstack;
          None
        )
      (* FIXME: we should use 1. to make sure the block is sampled,
       but the runtime does an infinite loop in native mode in this
       case. This bug will go away when the sampling of natively
       allocated will be correctly implemented. *)
        ~sampling_rate:0.5 ();
  allocate_arrays 250 250 100 false;
  stop ();
  match !callstack with
  | None -> assert false
  | Some cs -> Printexc.print_raw_backtrace stdout cs

let () = check_callstack ()

let () =
  Printf.printf "OK !\n"
