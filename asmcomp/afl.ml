let inst_ratio =
  match int_of_string (Sys.getenv "AFL_INST_RATIO") with
  | n when 1 <= n && n <= 100 -> n
  | n -> Misc.fatal_error "Bad value of AFL_INST_RATIO (must be between 1 and 100)"
  | exception Not_found -> 100
  | exception (Failure _) -> Misc.fatal_error "Bad value of AFL_INST_RATIO (must be integer)"

open Cmm

let afl_area_ptr = Cconst_symbol "__afl_area_ptr"
let afl_prev_loc = Cconst_symbol "__afl_prev_loc"

let map_size = 1 lsl 16

let rec with_logging b =
  if Random.int 100 >= inst_ratio then b else
  let instrumentation =
    let cur_location = Random.int map_size in
    let cur_pos = Ident.create "pos" in
    let afl_area = Ident.create "shared_mem" in
    Clet(afl_area, Cop(Cload Word, [afl_area_ptr]),
    Clet(cur_pos,  Cop(Cxor, [Cop(Cload Word, [afl_prev_loc]); Cconst_int cur_location]),
    Csequence(
      Cop(Cstore Byte_unsigned, [Cop(Cadda, [Cvar afl_area; Cvar cur_pos]);
                                 Cop(Cadda, [Cop (Cload Byte_unsigned, 
                                                  [Cop(Cadda, [Cvar afl_area; Cvar cur_pos])]);
                                             Cconst_int 1])]),
      Cop(Cstore Word, [afl_prev_loc; Cconst_int (cur_location lsr 1)])))) in
  Csequence(instrumentation, instrument b)
and instrument = function
  | Cifthenelse (cond, t, f) -> Cifthenelse (instrument cond, with_logging t, with_logging f)
  | Ccatch (nfail, ids, e1, e2) ->
     Ccatch (nfail, ids, instrument e1, with_logging e2)
  | Cloop e -> Cloop (with_logging e)
  | Ctrywith (e, ex, handler) -> Ctrywith (instrument e, ex, with_logging handler)
  | Cswitch (e, cases, handlers) -> Cswitch (instrument e, cases, Array.map instrument handlers)

  | Clet (v, e, body) -> Clet (v, instrument e, instrument body)
  | Cassign (v, e) -> Cassign (v, instrument e)
  | Ctuple es -> Ctuple (List.map instrument es)
  | Cop (op, es) -> Cop (op, List.map instrument es)
  | Csequence (e1, e2) -> Csequence (instrument e1, instrument e2)
  | Cexit (ex, args) -> Cexit (ex, List.map instrument args)

  | Cconst_int _
  | Cconst_natint _
  | Cconst_float _
  | Cconst_symbol _
  | Cconst_pointer _
  | Cconst_natpointer _
  | Cconst_blockheader _
  | Cvar _ as c -> c

