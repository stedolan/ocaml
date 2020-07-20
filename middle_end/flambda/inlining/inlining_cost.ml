(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

module DE = Simplify_envs.Downwards_env

let arch32 = Targetint.size = 32 (* are we compiling for a 32-bit arch *)
let arch64 = Targetint.size = 64 (* are we compiling for a 64-bit arch *)

(* Constants *)
(* CR-soon mshinwell: Investigate revised size numbers. *)

(* Native operations are estimated to be of size 1, this includes:
   - arithmetic operations
   - direct loads (without write barrier) *)

(** Allocation size *)
let alloc_size = 5

(* Call sizes are approximated, using for now the same values as
   flambda1. This estimation includes average cost of spilling
   registers. Typically, for a call, the number of arguments will be
   added to the size to take into account the likely move instructions
   needed before the call. *)
let direct_call_size = 4
let indirect_call_size = 6
let alloc_extcall_size = 10
let nonalloc_extcall_size = 4

(* Helper functions for computing sizes of primitives *)

let array_length_size (kind : Flambda_primitive.Array_kind.t) =
  match kind with
  | Float_array_opt_dynamic ->
    (* CR mshinwell: If Arch.size_float = Arch.size_addr then this should be
       the same as the other cases.  Unfortunately we haven't got access to
       Arch here I don't think, it would have to come via a [backend]
       parameter *)
    6
  | Immediates
  | Naked_floats
  | Values -> 2

let unary_int_prim_size kind op =
  match (kind : Flambda_kind.Standard_int.t),
        (op  : Flambda_primitive.unary_int_arith_op) with
  | Tagged_immediate, Neg -> 1
  | Tagged_immediate, Swap_byte_endianness ->
    2 + nonalloc_extcall_size + 1
  | Naked_immediate, Neg -> 1
  | Naked_immediate, Swap_byte_endianness ->
    nonalloc_extcall_size + 1
  | Naked_int64, Neg when arch32 ->
    nonalloc_extcall_size + 1
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Neg -> 1
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Swap_byte_endianness ->
    nonalloc_extcall_size + 1

let arith_conversion_size src dst =
  match (src : Flambda_kind.Standard_int_or_float.t),
        (dst : Flambda_kind.Standard_int_or_float.t) with
  (* 64-bit on 32-bit host specific cases *)
  | Naked_int64, Tagged_immediate
  | Naked_int64, Naked_int32
  | Naked_int64, (Naked_nativeint | Naked_immediate)
  | Naked_int64, Naked_float when arch32 ->
    nonalloc_extcall_size + 1 (* arg *)
  | Tagged_immediate, Naked_int64
  | Naked_int32, Naked_int64
  | (Naked_nativeint | Naked_immediate), Naked_int64
  | Naked_float, Naked_int64 when arch32 ->
    alloc_extcall_size + 1 (* arg *) + 1 (* unbox *)
  | Naked_float, Naked_float -> 0
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate),
    Tagged_immediate -> 1
  | Tagged_immediate,
    (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate) -> 1
  | Tagged_immediate, Tagged_immediate
  | Naked_int32, (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate)
  | Naked_int64, (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate)
  | Naked_nativeint,
    (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate)
  | Naked_immediate,
    (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate) -> 0
  | Tagged_immediate, Naked_float -> 1
  | (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint),
    Naked_float -> 1
  | Naked_float, Tagged_immediate -> 1
  | Naked_float,
    (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint) -> 1

let unbox_number kind =
  match (kind : Flambda_kind.Boxable_number.t) with
  | Naked_float -> 1 (* 1 load *)
  | Untagged_immediate -> 1 (* 1 shift *)
  | Naked_int64 when arch32 -> 4 (* 2 Cadda + 2 loads *)
  | Naked_int32 | Naked_int64 | Naked_nativeint -> 2 (* Cadda + load *)

let box_number kind =
  match (kind : Flambda_kind.Boxable_number.t) with
  | Naked_float -> alloc_size (* 1 alloc *)
  | Untagged_immediate -> 2 (* 1 shift + add *)
  | Naked_int32 when not arch32 -> 1 + alloc_size (* shift/sextend + alloc *)
  | Naked_int32 | Naked_int64 | Naked_nativeint -> alloc_size (* alloc *)

let block_load (kind : Flambda_primitive.Block_access_kind.t) =
  match kind with
  | Values _
  | Naked_floats _ -> 1

let array_load (kind : Flambda_primitive.Array_kind.t) =
  match kind with
  | Float_array_opt_dynamic -> 11  (* tag inspection etc. *)
  | Immediates -> 1  (* cadda + load *)
  | Naked_floats -> 1
  | Values -> 1

let block_set (kind : Flambda_primitive.Block_access_kind.t)
      (init : Flambda_primitive.Init_or_assign.t) =
  match kind, init with
  | Values _, (Assignment | Initialization) -> 1 (* cadda + store *)
  | Naked_floats _, (Assignment | Initialization) -> 1

let array_set (kind : Flambda_primitive.Array_kind.t)
      (_init : Flambda_primitive.Init_or_assign.t) =
  (* CR mshinwell: Check whether [init] should matter *)
  match kind with
  | Immediates -> 1  (* cadda + store *)
  | Naked_floats -> 1
  | Values -> 1
  | Float_array_opt_dynamic -> 16 (* tag inspection + rest.. *)

let string_or_bigstring_load kind width =
  let start_address_load =
    match (kind : Flambda_primitive.string_like_value) with
    | String | Bytes -> 0
    | Bigstring -> 2 (* add, load *)
  in
  let elt_load =
    match (width : Flambda_primitive.string_accessor_width) with
    | Eight ->
      3 (* untag, add, load *)

    (* CR gbury: these should actually depend on Arch.allow_unaligned_access,
       but that would add a dependency on the backend which is
       probably not desirable ? *)
    | Sixteen ->
      2 (* add, load (allow_unaligned_access) *)
      (* 7 (not allow_unaligned_access) *)
    | Thirty_two ->
      2 (* add, load (allow_unaligned_access) *)
      (* 17 (not allow_unaligned_access) *)
    | Sixty_four ->
      if arch32
      then nonalloc_extcall_size
      else 2 (* add, load (allow_unaligned_access) *)
      (* 37 (not allow_unaligned_access) *)
  in
  start_address_load + elt_load

(* This is exactly the same as string/bigstirng loads, since
   loads and stores have the same size *)
let bytes_like_set kind width =
  match (kind : Flambda_primitive.bytes_like_value) with
  | Bytes -> string_or_bigstring_load Bytes width
  | Bigstring -> string_or_bigstring_load Bigstring width

let binary_phys_comparison kind op =
  match (kind : Flambda_kind.t),
        (op : Flambda_primitive.equality_comparison) with
  (* int64 special case *)
  | Naked_number Naked_int64, Eq
  | Naked_number Naked_int64, Neq when arch32 ->
    1 (* untag *) + alloc_extcall_size + 2 (* args *)
    + 2 * (box_number Naked_int64)
  (* generic case *)
  | _, Eq -> 2
  | _, Neq -> 2

let divmod_bi_check else_branch_size bi =
  (* CR gbury: we should allow check Arch.division_crashed_on_overflow,
     but that's likely a dependency we want to avoid ? *)
  if (arch32 || bi <> Flambda_kind.Standard_int.Naked_int32)
  then 2 + else_branch_size
  else 0

let binary_int_arith_primitive kind op =
  match (kind : Flambda_kind.Standard_int.t),
        (op : Flambda_primitive.binary_int_arith_op) with
  (* Int64 bits ints on 32-bit archs *)
  | Naked_int64, Add
  | Naked_int64, Sub
  | Naked_int64, Mul when arch32 ->
    nonalloc_extcall_size + 2
  | Naked_int64, Div
  | Naked_int64, Mod when arch32 ->
    alloc_extcall_size + 2
  | Naked_int64, And
  | Naked_int64, Or
  | Naked_int64, Xor when arch32 ->
    nonalloc_extcall_size + 2
  (* Tagged integers *)
  | Tagged_immediate, Add -> 2
  | Tagged_immediate, Sub -> 2
  | Tagged_immediate, Mul -> 4
  | Tagged_immediate, Div -> 4
  | Tagged_immediate, Mod -> 4
  | Tagged_immediate, And -> 1
  | Tagged_immediate, Or  -> 1
  | Tagged_immediate, Xor -> 2
  (* Naked ints *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Add
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Sub
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Mul
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), And
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Or
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Xor ->
    1
  (* Division and modulo need some extra care *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Div ->
    divmod_bi_check 1 kind + 1
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Mod ->
    divmod_bi_check 0 kind + 1

let binary_int_shift_primitive kind op =
  match (kind : Flambda_kind.Standard_int.t),
        (op : Flambda_primitive.int_shift_op) with
  (* Int64 special case *)
  | Naked_int64, Lsl
  | Naked_int64, Lsr
  | Naked_int64, Asr when arch32 ->
    nonalloc_extcall_size + 2
  (* Int32 special case *)
  | Naked_int32, Lsr when arch64 -> 2
  (* Tagged integers *)
  | Tagged_immediate, Lsl -> 3
  | Tagged_immediate, Lsr -> 2
  | Tagged_immediate, Asr -> 2
  (* Naked ints *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Lsl
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Lsr
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Asr ->
    1

let binary_int_comp_primitive kind signed cmp =
  match (kind : Flambda_kind.Standard_int.t),
        (signed : Flambda_primitive.signed_or_unsigned),
        (cmp : Flambda_primitive.ordered_comparison) with
  | Naked_int64, Signed, Lt
  | Naked_int64, Signed, Le
  | Naked_int64, Signed, Gt
  | Naked_int64, Signed, Ge when arch32 ->
    alloc_extcall_size + 2
  | Naked_int64, Unsigned, (Lt | Le | Gt | Ge) when arch32 ->
    alloc_extcall_size + 2
  (* Tagged integers *)
  | Tagged_immediate, Signed, Lt
  | Tagged_immediate, Signed, Le
  | Tagged_immediate, Signed, Gt
  | Tagged_immediate, Signed, Ge
  | Tagged_immediate, Unsigned, Lt
  | Tagged_immediate, Unsigned, Le
  | Tagged_immediate, Unsigned, Gt
  | Tagged_immediate, Unsigned, Ge -> 2
  (* Naked integers. *)
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Signed, Lt
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Signed, Le
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Signed, Gt
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Signed, Ge
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Unsigned, Lt
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Unsigned, Le
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Unsigned, Gt
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Unsigned, Ge ->
    2

let binary_float_arith_primitive _op = 2

let binary_float_comp_primitive _op = 2


(* Primitives sizes *)

let unary_prim_size prim =
  match (prim : Flambda_primitive.unary_primitive) with
  | Duplicate_array _ | Duplicate_block _ -> alloc_extcall_size + 1
  | Is_int -> 1
  | Get_tag -> 2
  | Array_length kind -> array_length_size kind
  | Bigarray_length _ -> 2 (* cadda + load *)
  | String_length _ -> 5
  | Int_as_pointer -> 1
  | Opaque_identity -> 0
  | Int_arith (kind, op) -> unary_int_prim_size kind op
  | Float_arith _ -> 2
  | Num_conv { src; dst; } -> arith_conversion_size src dst
  | Boolean_not -> 1
  | Unbox_number k -> unbox_number k
  | Box_number k -> box_number k
  | Select_closure _ -> 1 (* caddv *)
  | Project_var _ -> 1 (* load *)

let binary_prim_size prim =
  match (prim : Flambda_primitive.binary_primitive) with
  | Block_load (kind, _) -> block_load kind
  | Array_load (kind, _mut) -> array_load kind
  | String_or_bigstring_load (kind, width) ->
    string_or_bigstring_load kind width
  | Bigarray_load (_dims, (Complex32 | Complex64) , _layout) ->
    5 (* ~ 5 block_loads *) + alloc_size (* complex allocation *)
  | Bigarray_load (_dims, _kind, _layout) ->
    2 (* ~ 2 block loads *)
  | Phys_equal (kind, op) ->
    binary_phys_comparison kind op
  | Int_arith (kind, op) ->
    binary_int_arith_primitive kind op
  | Int_shift (kind, op) ->
    binary_int_shift_primitive kind op
  | Int_comp (kind, signed, cmp) ->
    binary_int_comp_primitive kind signed cmp
  | Float_arith op ->
    binary_float_arith_primitive op
  | Float_comp cmp ->
    binary_float_comp_primitive cmp

let ternary_prim_size prim =
  match (prim : Flambda_primitive.ternary_primitive) with
  | Block_set (block_access, init) -> block_set block_access init
  | Array_set (kind, init) -> array_set kind init
  | Bytes_or_bigstring_set (kind, width) -> bytes_like_set kind width
  | Bigarray_set (_dims, (Complex32 | Complex64), _layout) ->
    5 (* ~ 3 block_load + 2 block_set *)
  | Bigarray_set (_dims, _kind, _layout) ->
    2 (* ~ 1 block_load + 1 block_set *)

let variadic_prim_size prim args =
  match (prim : Flambda_primitive.variadic_primitive) with
  | Make_block (_, _mut)
  (* CR mshinwell: I think Make_array for a generic array ("Anything") is
     more expensive than the other cases *)
  | Make_array (_, _mut) -> alloc_size + List.length args

let prim_size (prim : Flambda_primitive.t) =
  match prim with
  | Unary (p, _) -> unary_prim_size p
  | Binary (p, _, _) -> binary_prim_size p
  | Ternary (p, _, _, _) -> ternary_prim_size p
  | Variadic (p, args) -> variadic_prim_size p args

(* Simple approximation of the space cost of an Flambda expression. *)

let smaller' denv expr ~than:threshold =
  let size = ref 0 in
  let rec expr_size denv expr =
    if !size > threshold then raise Exit;
    match Expr.descr expr with
    | Let let_expr ->
      named_size denv (Let.defining_expr let_expr);
      Let.pattern_match let_expr
        ~f:(fun _bindable_let_bound ~body -> expr_size denv body)
    | Let_cont (Non_recursive { handler; _ }) ->
      Non_recursive_let_cont_handler.pattern_match handler
        ~f:(fun _cont ~body -> expr_size denv body);
      (* CR mshinwell: Move this next line into the above *)
      continuation_handler_size denv
        (Non_recursive_let_cont_handler.handler handler)
    | Let_cont (Recursive handlers) ->
      Recursive_let_cont_handlers.pattern_match handlers
        ~f:(fun ~body handlers ->
          expr_size denv body;
          let handlers = Continuation_handlers.to_map handlers in
          Continuation.Map.iter (fun _cont handler ->
              continuation_handler_size denv handler)
            handlers)
    | Apply apply ->
      let call_cost =
        match Apply.call_kind apply with
        | Function Direct _ -> direct_call_size
        (* CR mshinwell: Check / fix these numbers *)
        | Function Indirect_unknown_arity -> indirect_call_size
        | Function Indirect_known_arity _ -> indirect_call_size
        | C_call { alloc = true; _ } -> alloc_extcall_size
        | C_call { alloc = false; _ } -> nonalloc_extcall_size
        | Method _ -> 8 (* from flambda/inlining_cost.ml *)
      in
      size := !size + call_cost
    | Apply_cont e ->
      begin match Apply_cont.trap_action e with
      | None -> ()
      | Some (Push _ | Pop _) -> size := !size + 4
      end;
      incr size
    | Switch switch -> size := !size + (5 * Switch.num_arms switch)
    | Invalid _ -> ()
  and named_size denv (named : Named.t) =
    if !size > threshold then raise Exit;
    match named with
    | Simple simple ->
      Simple.pattern_match simple
        ~const:(fun _ -> incr size)
        ~name:(fun _ -> ())
    | Set_of_closures set_of_closures ->
      (* Mimic Closure: do not inline functions containing closures.  Note that
         there is an additional check for closure-containing functions in
         [Closure_conversion], to try to replicate Closure's behaviour as
         closely as possible. *)
      if Flambda_features.Expert.fallback_inlining_heuristic () then begin
        raise Exit
      end;
      let func_decls = Set_of_closures.function_decls set_of_closures in
      let funs = Function_declarations.funs func_decls in
      Closure_id.Map.iter (fun _ func_decl ->
          let code_id = Function_declaration.code_id func_decl in
          let params_and_body = DE.find_code denv code_id in
          Function_params_and_body.pattern_match params_and_body
            ~f:(fun ~return_continuation:_ _exn_continuation _params
                    ~body ~my_closure:_ ->
              expr_size denv body))
        funs
    | Prim (prim, _dbg) ->
      size := !size + prim_size prim
    | Static_const _ -> ()
  and continuation_handler_size denv handler =
    let params_and_handler = Continuation_handler.params_and_handler handler in
    Continuation_params_and_handler.pattern_match params_and_handler
      ~f:(fun _params ~handler -> expr_size denv handler)
  in
  try
    expr_size denv expr;
    if !size <= threshold then Some !size
    else None
  with Exit ->
    None

let size denv expr =
  match smaller' denv expr ~than:max_int with
  | Some size -> size
  | None ->
    (* There is no way that an expression of size max_int could fit in
       memory. *)
    assert false  (* CR mshinwell: this should not be an assertion *)

(*
let sizes exprs =
  List.fold_left (fun total expr -> total + size expr) 0 exprs
*)

module Threshold = struct
  type t =
    | Never_inline
    | Can_inline_if_no_larger_than of int

  let add t1 t2 =
    match t1, t2 with
    | Never_inline, t -> t
    | t, Never_inline -> t
    | Can_inline_if_no_larger_than i1, Can_inline_if_no_larger_than i2 ->
        Can_inline_if_no_larger_than (i1 + i2)

  let sub t1 t2 =
    match t1, t2 with
    | Never_inline, _ -> Never_inline
    | t, Never_inline -> t
    | Can_inline_if_no_larger_than i1, Can_inline_if_no_larger_than i2 ->
        if i1 > i2 then Can_inline_if_no_larger_than (i1 - i2)
        else Never_inline

  let min t1 t2 =
    match t1, t2 with
    | Never_inline, _ -> Never_inline
    | _, Never_inline -> Never_inline
    | Can_inline_if_no_larger_than i1, Can_inline_if_no_larger_than i2 ->
      Can_inline_if_no_larger_than (min i1 i2)
end

let smaller denv lam ~than =
  smaller' denv lam ~than <> None

let can_inline denv lam inlining_threshold ~bonus =
  match inlining_threshold with
  | Threshold.Never_inline -> false
  | Threshold.Can_inline_if_no_larger_than inlining_threshold ->
     smaller denv
       lam
       ~than:(inlining_threshold + bonus)

let cost (flag : Clflags.Int_arg_helper.parsed) ~round =
  Clflags.Int_arg_helper.get ~key:round flag

let benefit_factor = 1

module Benefit = struct
  type t = {
    remove_call : int;
    remove_alloc : int;
    remove_prim : int;
    remove_branch : int;
    (* CR-someday pchambart: branch_benefit : t list; *)
    direct_call_of_indirect : int;
    requested_inline : int;
    (* Benefit to compensate the size of functions marked for inlining *)
  }

  let zero = {
    remove_call = 0;
    remove_alloc = 0;
    remove_prim = 0;
    remove_branch = 0;
    direct_call_of_indirect = 0;
    requested_inline = 0;
  }

  let remove_call t = { t with remove_call = t.remove_call + 1; }
  let remove_alloc t = { t with remove_alloc = t.remove_alloc + 1; }

  let add_primitive _prim t =
    { t with remove_prim = t.remove_prim - 1; }

  let remove_primitive _prim t =
    { t with remove_prim = t.remove_prim + 1; }

  let remove_primitive_application _prim t =
    { t with remove_prim = t.remove_prim + 1; }

  let remove_branch t = { t with remove_branch = t.remove_branch + 1; }

  let direct_call_of_indirect_known_arity t =
    { t with direct_call_of_indirect = t.direct_call_of_indirect + 1; }

  let direct_call_of_indirect_unknown_arity t =
    { t with direct_call_of_indirect = t.direct_call_of_indirect + 1; }

  let requested_inline denv t ~size_of =
    let size = size denv size_of in
    { t with requested_inline = t.requested_inline + size; }

  let print ppf b =
    Format.fprintf ppf "@[remove_call: %i@ remove_alloc: %i@ \
                        remove_prim: %i@ remove_branch: %i@ \
                        direct: %i@ requested: %i@]"
      b.remove_call
      b.remove_alloc
      b.remove_prim
      b.remove_branch
      b.direct_call_of_indirect
      b.requested_inline

  let evaluate t ~round : int =
    benefit_factor *
      (t.remove_call * (cost !Clflags.inline_call_cost ~round)
       + t.remove_alloc * (cost !Clflags.inline_alloc_cost ~round)
       + t.remove_prim * (cost !Clflags.inline_prim_cost ~round)
       + t.remove_branch * (cost !Clflags.inline_branch_cost ~round)
       + (t.direct_call_of_indirect
         * (cost !Clflags.inline_indirect_cost ~round)))
    + t.requested_inline

  let (+) t1 t2 = {
    remove_call = t1.remove_call + t2.remove_call;
    remove_alloc = t1.remove_alloc + t2.remove_alloc;
    remove_prim = t1.remove_prim + t2.remove_prim;
    remove_branch = t1.remove_branch + t2.remove_branch;
    direct_call_of_indirect =
      t1.direct_call_of_indirect + t2.direct_call_of_indirect;
    requested_inline = t1.requested_inline + t2.requested_inline;
  }

(*
  let (-) t1 t2 = {
    remove_call = t1.remove_call - t2.remove_call;
    remove_alloc = t1.remove_alloc - t2.remove_alloc;
    remove_prim = t1.remove_prim - t2.remove_prim;
    remove_branch = t1.remove_branch - t2.remove_branch;
    direct_call_of_indirect =
      t1.direct_call_of_indirect - t2.direct_call_of_indirect;
    requested_inline = t1.requested_inline - t2.requested_inline;
  }
*)

  let max ~round t1 t2 =
    let c1 = evaluate ~round t1 in
    let c2 = evaluate ~round t2 in
    if c1 > c2 then t1 else t2

(*
  let add_code lam b =
    b - (remove_code lam zero)

  let add_code_named lam b =
    b - (remove_code_named lam zero)
*)
end

let scale_inline_threshold_by = 8
