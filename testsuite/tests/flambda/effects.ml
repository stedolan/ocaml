(* TEST
* flambda
** native
*)

(* This test tries and ensure that allocations (such as closure allocation,
   regular value allocations, etc...), are not commuted with function calls,
   such as calls to GC functions that can observe the mutable state of
   the GC. *)

external minor_words : unit -> (float [@unboxed])
  = "caml_gc_minor_words" "caml_gc_minor_words_unboxed"

let[@inline never] f x foo =
  let before = minor_words () in
  let y = x * 2 in
  let after = minor_words () in
  let diff = after -. before in
  foo (Some y) (fun () -> y) diff


let () =
  let x = f 0 (fun _ _ diff -> diff) in
  print_float x

