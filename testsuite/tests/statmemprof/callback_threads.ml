(* TEST

* hassysthreads
include systhreads
** bytecode
** native

*)

open Gc.Memprof

let slow_alloc () =
  for i = 1 to 10 do
    print_string "alloc\n";
    ignore (Sys.opaque_identity (Array.make 200 0));
    Unix.sleepf 0.07;
  done

let active_callbacks = ref 0

let _ =
  start {
    (* FIXME: we should use 1. to make sure the block is sampled,
       but the runtime does an infinite loop in native mode in this
       case. This bug will go away when the sampling of natively
       allocated will be correctly implemented. *)
    sampling_rate = 0.5;
    callstack_size = 10;
    callback = (fun _ ->
      print_string "callback\n";
      Unix.sleepf 0.1;
      print_string "callback done\n"; None)
  };
  let t = Thread.create slow_alloc () in
  slow_alloc ();
  Thread.join t;
  stop ()
