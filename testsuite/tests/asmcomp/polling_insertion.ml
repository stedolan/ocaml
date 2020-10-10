(* TEST
   modules = "polling.c"
   compare_programs = "false"
   * native
*)

(* This set of tests examine poll insertion behaviour. We do this by requesting
   and checking the number of minor collections at various points to determine
   whether a poll was correctly added. There are some subtleties because 
   [caml_empty_minor_heap] will not increment the minor_collections stat if
   nothing has been allocated on the minor heap, so we sometimes need to
   add an allocation before we call [request_minor_gc]. The [minor_gcs]
   function returns the number of minor collections so far without allocating.

   ignore(Sys.opaque_identity(ref 41)) is used wherever we want to do an
   allocation in order to use some minor heap in order for the minor
   collections stat to be incremenets.

   ignore(Sys.opaque_identity(ref 42)) is used wherever we want an allocation
   for the purposes of testing whether a poll would be elided or not.
*)

external request_minor_gc : unit -> unit = "request_minor_gc"
external minor_gcs : unit -> int = "minor_gcs"

(* This function tests that polls are added only to the back edges of loops *)
let polls_added_to_loops () =
  let minors_before = minor_gcs () in
  request_minor_gc ();
  for a = 0 to 1 do
    let minors_now = minor_gcs () in
    if a = 0 then
      (* No polls on the entry to the loop *)
      assert (minors_before == minors_now)
    else
      (* We should have hit a poll on the jump at the end of the
         first iteration *)
      assert (minors_before + 1 == minors_now)
  done

(* This next pair of functions test that polls are added to the prologue 
   of a function. We need a loop in this function to avoid the poll getting 
   removed by the leaf function optimisation *)
let func_with_added_poll_because_loop () =
  (* the loop here means this is not treated as a leaf *)
  for a = 0 to Sys.opaque_identity(0) do
    ignore (Sys.opaque_identity 42)
  done
  [@@inline never]

let func_with_added_poll_because_call () =
  (* the call here means this is not treated at a leaf *)
  ignore(Sys.opaque_identity(minor_gcs ()))
  [@@inline never]

let func_with_added_poll_because_allocation_is_conditional n =
  (* the call here means this is not treated at a leaf *)
  ignore(Sys.opaque_identity(minor_gcs ()));
  (* since this is not a leaf, it will have a poll if there are
     no unconditional allocations. We use the if here to ensure
     the allocation is conditional. *)
  if n = 0 then
    ignore(Sys.opaque_identity(ref 42))
  else
    ()
  [@@inline never]

let polls_added_to_functions () =
  ignore(Sys.opaque_identity(ref 41));
  let minors_before = minor_gcs () in
  request_minor_gc ();
  func_with_added_poll_because_loop ();
  let minors_now = minor_gcs () in
  assert (minors_before + 1 == minors_now);

  ignore(Sys.opaque_identity(ref 41));
  let minors_before = minor_gcs () in
  request_minor_gc ();
  func_with_added_poll_because_call ();
  let minors_now = minor_gcs () in
  assert (minors_before +1 == minors_now);

  ignore(Sys.opaque_identity(ref 41));
  let minors_before = minor_gcs () in
  request_minor_gc ();
  func_with_added_poll_because_allocation_is_conditional 1;
  let minors_now = minor_gcs () in
  assert (minors_before +1 == minors_now)

(* These next functions test that polls are not added to functions that
   unconditionally allocate. We need the empty loop to avoid these functions
   being treated as leaf functions.
   [allocating_func] allocates unconditionally
   [allocating_func_if] allocates unconditionally but does so
   on two separate branches *)
let allocating_func minors_before =
  let minors_now = minor_gcs () in
  assert (minors_before == minors_now);
  (* No poll yet *)
  ignore (Sys.opaque_identity (ref 42));
  let minors_now2 = minor_gcs () in
  assert (minors_before + 1 == minors_now2);
  (* Polled at alloc *)
  for a = 0 to 0 do
    ignore (Sys.opaque_identity 0)
  done
  [@@inline never]

let allocating_func_if minors_before =
  let minors_now = minor_gcs () in
  assert (minors_before == minors_now);
  (* No poll yet *)
  if minors_before > 0 then ignore (Sys.opaque_identity (ref 42))
  else ignore (Sys.opaque_identity (ref 42));
  let minors_now2 = minor_gcs () in
  assert (minors_before + 1 == minors_now2);
  (* Polled at alloc *)
  for a = 0 to 0 do
    ignore (Sys.opaque_identity 0)
  done
  [@@inline never]

let allocating_func_nested_ifs minors_before =
  let minors_now = minor_gcs () in
  assert (minors_before == minors_now);
  (* No poll yet *)
  if minors_before > 0 then 
    if minors_before > 1 then
      ignore (Sys.opaque_identity (ref 42))
    else
      ignore (Sys.opaque_identity (ref 42))
  else 
    if minors_before < 5 then
      ignore (Sys.opaque_identity (ref 42))
    else
      ignore (Sys.opaque_identity (ref 42));
  let minors_now2 = minor_gcs () in
  assert (minors_before + 1 == minors_now2);
  (* Polled at alloc *)
  for a = 0 to 0 do
    ignore (Sys.opaque_identity 0)
  done
  [@@inline never]  

let allocating_func_match minors_before =
  let minors_now = minor_gcs () in
  assert (minors_before == minors_now);
  (* No poll yet *)
  match minors_before with
  | 0 -> ignore (Sys.opaque_identity (ref 42))
  | _ -> ignore (Sys.opaque_identity (ref 42));
  let minors_now2 = minor_gcs () in
  assert (minors_before + 1 == minors_now2);
  (* Polled at alloc *)
  for a = 0 to 0 do
    ignore (Sys.opaque_identity 0)
  done
  [@@inline never]

let polls_not_added_unconditionally_allocating_functions () =
  let minors_before = minor_gcs () in
  ignore(Sys.opaque_identity(ref 41));
  request_minor_gc ();
  allocating_func minors_before;
  let minors_before = minor_gcs () in
  ignore(Sys.opaque_identity(ref 41));
  request_minor_gc ();
  allocating_func_if minors_before;
  let minors_before = minor_gcs () in
  ignore(Sys.opaque_identity(ref 41));
  request_minor_gc ();
  allocating_func_nested_ifs minors_before;
  let minors_before = minor_gcs () in
  ignore(Sys.opaque_identity(ref 41));
  request_minor_gc ();
  allocating_func_match minors_before

(* This function tests that polls are not added to the back edge of
   where loop bodies allocat unconditionally *)
let polls_not_added_to_allocating_loops () =
  let current_minors = ref (minor_gcs ()) in
  request_minor_gc ();
  for a = 0 to 1 do
    (* Since the loop body allocates there should be no poll points *)
    let minors_now = minor_gcs () in
      assert(minors_now == !current_minors);
      ignore(Sys.opaque_identity(ref 42));
      let minors_now2 = minor_gcs () in
        assert(minors_now+1 == minors_now2);
        current_minors := minors_now2;
        ignore(Sys.opaque_identity(ref 41));
        request_minor_gc ()
  done

(* this next function checks that leaf functions do not have polls
   inserted. A leaf function here is one that makes no calls
   (including tail calls) and has no loops. *)
let leaf_func () =
  ignore(Sys.opaque_identity 0)

let polls_not_added_to_leaf_functions () =
  let minors_before = minor_gcs () in
  request_minor_gc ();
  leaf_func ();
  let minors_now = minor_gcs () in
  assert(minors_before == minors_now)

(* this next set of functions tests that tail recursive functions
   have polls added correctly *)
let rec rec_func n =
  match n with
  | 0 -> 0
  | _ -> begin
    ignore(Sys.opaque_identity(ref 41)); (* need to use _some_ minor heap *)
    request_minor_gc (); 
    (rec_func[@tailcall]) (n-1)
  end

let polls_added_to_recursive_functions () =
  let minors_before = minor_gcs () in
    ignore(rec_func 5);
    let minors_after = minor_gcs () in
      assert(minors_before+5 == minors_after)


(* this set of functions tests whether polls are added before raises *)
exception TestException

let func_that_raises () =
  raise TestException

let polls_added_before_raises () =
  let minors_before = minor_gcs () in
    request_minor_gc ();
    try 
      func_that_raises ()
    with TestException ->
      let minors_after = minor_gcs () in
        assert(minors_before+1 == minors_after)

let () =
  polls_added_to_loops (); (* relies on there being some minor heap usage *)
  ignore(Sys.opaque_identity(ref 41));
  polls_added_to_functions ();
  ignore(Sys.opaque_identity(ref 41));
  polls_added_to_recursive_functions ();
  ignore(Sys.opaque_identity(ref 41));
  polls_not_added_unconditionally_allocating_functions ();
  ignore(Sys.opaque_identity(ref 41));
  polls_not_added_to_allocating_loops ();
  ignore(Sys.opaque_identity(ref 41));
  polls_not_added_to_leaf_functions ();
  ignore(Sys.opaque_identity(ref 41));
  polls_added_before_raises ()
