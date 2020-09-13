open Mach

let add_iteration_counter_before (f : Mach.instruction) counter_reg : Mach.instruction =
  {
    desc = Iop (Iconst_int 32n);
    next = f;
    arg = Array.make 0 Reg.dummy;
    res = [| counter_reg |];
    dbg = f.dbg;
    live = Reg.Set.empty;
    available_before = f.available_before;
    available_across = f.available_across;
  }

(*let add_poll_before (f : Mach.instruction) : Mach.instruction =
  let new_live = Reg.Set.union f.live (Reg.Set.of_seq (Array.to_seq f.arg)) in
  {
    desc = Iop Ipoll;
    next = f;
    arg = Array.make 0 Reg.dummy;
    res = Array.make 0 Reg.dummy;
    dbg = f.dbg;
    live = new_live;
    available_before = f.available_before;
    available_across = f.available_across;
  }*)

let add_conditional_poll_before_exit (f : Mach.instruction) (counter_reg: Reg.t) : Mach.instruction =
  let poll_branch = {
    desc = Iop Ipoll;
    next = Mach.end_instr ();
    arg = Array.make 0 Reg.dummy;
    res = Array.make 0 Reg.dummy;
    dbg = f.dbg;
    live = Reg.Set.empty;
    available_before = f.available_before;
    available_across = f.available_across;
  } in
  let new_if = Mach.instr_cons (Iifthenelse (Iinttest_imm (Iunsigned (Ceq), 0), poll_branch, Mach.end_instr ())) [| counter_reg |] [||] f in
    let sub = Mach.instr_cons (Iop(Iintop_imm(Isub, 1))) [| counter_reg |] [| counter_reg |] new_if in
      sub

type allocation_result = Allocation | NoAllocation | Exited

let combine_paths p0 p1 = match p0, p1 with Allocation, Allocation -> Allocation
| Exited, _ | _, Exited -> Exited
| NoAllocation, _ | _, NoAllocation -> NoAllocation

let rec reduce_paths_array arr =
  let rec red_arr acc arr n =
    match n with 
    | 0 -> acc
    | _ ->
      let curr_path = check_path arr.(n) in
        let new_acc = match acc with 
        | None -> curr_path
        | Some(v) -> combine_paths v curr_path
        in red_arr (Some new_acc) arr (n-1)
  in 
  let res = red_arr None arr ((Array.length arr)-1) in
    match res with
    | None -> NoAllocation
    | Some(v) -> v
and reduce_paths_list l =
  let rec red_list acc l =
    match l with 
    | [] -> acc
    | (h :: tl) ->
      let curr_path = check_path h in
        let new_acc = match acc with 
        | None -> curr_path
        | Some(v) -> combine_paths v curr_path
        in red_list (Some new_acc) tl
  in 
  let res = red_list None l in
    match res with
    | None -> NoAllocation
    | Some(v) -> v
and check_path (f : Mach.instruction) : allocation_result =
  match f.desc with
  | Iifthenelse (_, i0, i1) ->
      (match combine_paths (check_path i0) (check_path i1) with
      | NoAllocation -> check_path f.next
      | pv -> pv)
  | Iswitch (_, cases) ->
      let case_state = reduce_paths_array cases in
      (match case_state with
      | NoAllocation -> check_path f.next
      | pv -> pv)
  | Icatch (_, handlers, body) ->
      let handlers_state = reduce_paths_list (List.map (fun (_,h) -> h) handlers) in
      (match combine_paths handlers_state (check_path body) with
      | NoAllocation -> check_path f.next
      | pv -> pv)
  | Itrywith (body, handler) ->
      (match combine_paths (check_path body) (check_path handler) with
      | NoAllocation -> check_path f.next
      | pv -> pv)
  | Ireturn
  | Iop (Itailcall_ind _)
  | Iop (Itailcall_imm _)
  | Iraise _ ->
      Exited
  | Iend | Iexit _ -> NoAllocation
  | Iop (Ialloc _) -> Allocation
  | Iop _ -> check_path f.next

(* this determines whether from a given instruction we unconditionally
   allocate and this is used to avoid adding polls unnecessarily *)
let allocates_unconditionally (i : Mach.instruction) =
  match check_path i with
  | Allocation -> true
  | NoAllocation | Exited -> false

let is_leaf_func_without_loops (fun_body : Mach.instruction) =
  let rec contains_calls_or_loops (i : Mach.instruction) =
  match i.desc with
  | Iifthenelse (_, ifso, ifnot) ->
    (contains_calls_or_loops ifso) || (contains_calls_or_loops ifnot) || contains_calls_or_loops i.next
  | Iswitch (_, cases) ->
    (Array.exists (fun c -> contains_calls_or_loops c) cases) || contains_calls_or_loops i.next
  | Icatch (rec_flag, handlers, body) ->
    begin
      match rec_flag with 
    | Recursive ->
      true
    | Nonrecursive ->
      (List.exists (fun (_, h) -> contains_calls_or_loops h) handlers) || contains_calls_or_loops body || contains_calls_or_loops i.next
    end
  | Itrywith (body, handler) ->
    (contains_calls_or_loops body) || (contains_calls_or_loops handler) || contains_calls_or_loops i.next
  | Iend -> false
  | Iop(Iextcall _ | Icall_ind _ | Icall_imm _ | Itailcall_imm _ | Itailcall_ind _) ->
      true
  | Ireturn | Iexit _ | Iraise _ -> false
  | Iop _ -> contains_calls_or_loops i.next
  in not(contains_calls_or_loops fun_body)

(* finds_rec_handlers *)
let rec find_rec_handlers (f : Mach.instruction) =
  match f.desc with
  | Iifthenelse (_, ifso, ifnot) ->
      let ifso_rec_handlers = find_rec_handlers ifso in
      let ifnot_rec_handlers = find_rec_handlers ifnot in
      let next_rec_handlers = find_rec_handlers f.next in
        ifso_rec_handlers @ ifnot_rec_handlers @ next_rec_handlers
  | Iswitch (_, cases) ->
      let case_rec_handlers =
        Array.fold_left
          (fun agg_rec_handlers case -> agg_rec_handlers @ (find_rec_handlers case))
          [] cases
      in
      case_rec_handlers @ (find_rec_handlers f.next)
  | Icatch (rec_flag, handlers, body) -> (
      match rec_flag with
      | Recursive ->
          let rec_handlers =
            List.map
              (fun (id, handler) ->
                let inner_rec_handlers = find_rec_handlers handler in
                let current_rec_handlers = if not (allocates_unconditionally handler) then
                  [(id, Reg.create Int)]
                else [] in
                inner_rec_handlers @ current_rec_handlers)
              handlers |> List.flatten
          in
          let body_rec_handlers = find_rec_handlers body in
            body_rec_handlers @ rec_handlers @ (find_rec_handlers f.next)
      | Nonrecursive ->
          let non_rec_catch_handlers =
            List.fold_left
              (fun tmp_rec_handlers (_, handler) ->
                tmp_rec_handlers @ (find_rec_handlers handler))
              [] handlers
          in
          let body_rec_handlers = find_rec_handlers body in
            body_rec_handlers @ non_rec_catch_handlers @ (find_rec_handlers f.next))
  | Itrywith (body, handler) ->
      let handler_rec_handler = find_rec_handlers handler in
      let body_rec_handlers = find_rec_handlers body in
        body_rec_handlers @ handler_rec_handler @ (find_rec_handlers f.next)
  | Iexit _ | Iend | Ireturn
  | Iop (Itailcall_ind _)
  | Iop (Itailcall_imm _)
  | Iraise _ ->
      []
  | Iop _ -> find_rec_handlers f.next

let instrument_loops_with_polls (rec_handlers : (int * Reg.t) list) (i : Mach.instruction) =
  let rec instrument_loops (f : Mach.instruction) =
  match f.desc with
  | Iifthenelse (test, i0, i1) ->
      {
        f with
        desc =
          Iifthenelse (test, instrument_loops i0, instrument_loops i1);
        next = instrument_loops f.next;
      }
  | Iswitch (index, cases) ->
      {
        f with
        desc = Iswitch (index, Array.map instrument_loops cases);
        next = instrument_loops f.next;
      }
  | Icatch (rec_flag, handlers, body) ->
    let new_f = {
        f with
        desc =
          Icatch
            ( rec_flag,
              List.map
                (fun (idx, instrs) -> (idx, instrument_loops instrs))
                handlers,
              instrument_loops body );
        next = instrument_loops f.next;
    } in
    (match rec_flag with 
    | Recursive ->
      List.fold_left (fun fa (i, _) -> 
        match List.assoc_opt i rec_handlers with
        | Some(reg) -> add_iteration_counter_before fa reg
        | None -> fa)
      new_f handlers
    | Nonrecursive ->
      new_f)
  | Itrywith (body, handler) ->
      {
        f with
        desc =
          Itrywith (instrument_loops body, instrument_loops handler);
        next = instrument_loops f.next;
      }
  | Iexit id ->
      let new_f = { f with next = instrument_loops f.next } in
      begin
        match List.assoc_opt id rec_handlers with 
        | Some(counter_reg) -> add_conditional_poll_before_exit new_f counter_reg
        | None -> new_f
      end
  | Iend | Ireturn | Iop (Itailcall_ind _) | Iop (Itailcall_imm _) | Iraise _ ->
      f
  | Iop _ -> { f with next = instrument_loops f.next }
  in instrument_loops i

let funcdecl (i : Mach.fundecl) : Mach.fundecl =
  let f = i.fun_body in
  let rec_handlers = find_rec_handlers f in
  { i with fun_body = instrument_loops_with_polls rec_handlers f }

(* What you need to do next is in the catch, you need to check if it's rec and if it is rec
   then for each of the handler ids you need to initialise a constant before the catch *)