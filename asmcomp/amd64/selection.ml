(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Instruction selection for the AMD64 *)

open Arch
open Proc
open Cmm
open Mach
open Selectgen

(* Auxiliary for recognizing addressing modes *)

type addressing_expr =
    Asymbol of string
  | Alinear of expression
  | Aadd of expression * expression
  | Ascale of expression * int
  | Ascaledadd of expression * expression * int

let rec select_addr exp =
  match exp with
    Cconst_symbol s when not !Clflags.dlcode ->
      (Asymbol s, 0)
  | Cop((Caddi | Cadda), [arg; Cconst_int m]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop((Csubi | Csuba), [arg; Cconst_int m]) ->
      let (a, n) = select_addr arg in (a, n - m)
  | Cop((Caddi | Cadda), [Cconst_int m; arg]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop(Clsl, [arg; Cconst_int(1|2|3 as shift)]) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, 1 lsl shift), n lsl shift)
      | _ -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [arg; Cconst_int(2|4|8 as mult)]) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | _ -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [Cconst_int(2|4|8 as mult); arg]) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | _ -> (Alinear exp, 0)
      end
  | Cop((Caddi | Cadda), [arg1; arg2]) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1), (Alinear e2, n2)) ->
              (Aadd(e1, e2), n1 + n2)
        | ((Alinear e1, n1), (Ascale(e2, scale), n2)) ->
              (Ascaledadd(e1, e2, scale), n1 + n2)
        | ((Ascale(e1, scale), n1), (Alinear e2, n2)) ->
              (Ascaledadd(e2, e1, scale), n1 + n2)
        | (_, (Ascale(e2, scale), n2)) ->
              (Ascaledadd(arg1, e2, scale), n2)
        | ((Ascale(e1, scale), n1), _) ->
              (Ascaledadd(arg2, e1, scale), n1)
        | _ ->
              (Aadd(arg1, arg2), 0)
      end
  | arg ->
      (Alinear arg, 0)

(* Special constraints on operand and result registers *)

exception Use_default

let rax = phys_reg 0
let rcx = phys_reg 5
let rdx = phys_reg 4

let pseudoregs_for_operation op arg res =
  match op, arg with
  (* Two-address binary operations: arg.(0) and res.(0) must be the same *)
  | Iintop(Iadd|Isub|Imul|Iand|Ior|Ixor), _ 
  | Iintop(Ilsl|Ilsr|Iasr), [| _; Oimm _ |] 
  | (Iaddf|Isubf|Imulf|Idivf), _ ->
      ([|Oreg res.(0); arg.(1)|], res)
  (* One-address unary operations: arg.(0) and res.(0) must be the same *)
  | Iabsf, _ | Inegf, _ ->
      ([| Oreg res.(0) |], res)
  (* For shifts with variable shift count, second arg must be in rcx *)
  | Iintop(Ilsl|Ilsr|Iasr), _ ->
      ([|Oreg res.(0); Oreg rcx|], res)
  (* For div and mod with immediate operand, arg must not be in rax.
     Keep it simple, force it in rdx. *)
  | Iintop(Idiv|Imod), [| _; Oimm _ as imm|] ->
      ([| Oreg rdx; imm |], [| rdx |])
  (* For div and mod, first arg must be in rax, rdx is clobbered,
     and result is in rax or rdx respectively.
     Keep it simple, just force second argument in rcx. *)
  | Iintop(Idiv), _ ->
      ([| Oreg rax; Oreg rcx |], [| rax |])
  | Iintop(Imod), _ ->
      ([| Oreg rax; Oreg rcx |], [| rdx |])
  (* Other instructions are regular *)
  | _ -> raise Use_default

(* The selector class *)

class selector = object (self)

inherit Selectgen.selector_generic as super

method is_immediate n = n <= 0x7FFFFFFF && n >= -0x80000000

method is_immediate_natint n = n <= 0x7FFFFFFFn && n >= -0x80000000n

method select_addressing chunk exp =
  let (a, d) = select_addr exp in
  (* PR#4625: displacement must be a signed 32-bit immediate *)
  if d < -0x8000_0000 || d > 0x7FFF_FFFF
  then (Iindexed 0, exp)
  else match a with
    | Asymbol s ->
        (Ibased(s, d), Ctuple [])
    | Alinear e ->
        (Iindexed d, e)
    | Aadd(e1, e2) ->
        (Iindexed2 d, Ctuple[e1; e2])
    | Ascale(e, scale) ->
        (Iscaled(scale, d), e)
    | Ascaledadd(e1, e2, scale) ->
        (Iindexed2scaled(scale, d), Ctuple[e1; e2])

method! select_store exp =
  match exp with
    (Cconst_int n | Cconst_pointer n) when self#is_immediate n ->
      (Istore Word, Oper_mach (Oimm (Nativeint.of_int n)))
  | (Cconst_natint n | Cconst_natpointer n) when self#is_immediate_natint n ->
      (Istore Word, Oper_mach (Oimm n))
  | Cconst_symbol s when not (!pic_code || !Clflags.dlcode) ->
      (Ispecific(Istore_symbol(s)), Oper_val (Ctuple []))
  | _ ->
      super#select_store exp

method! select_operation op args =
  match op, args with
  (* Recognize the LEA instruction *)
    (Caddi | Cadda | Csubi | Csuba), _ ->
      begin match self#select_addressing Word (Cop(op, args)) with
        (Iindexed d, _) -> super#select_operation op args
      | (Iindexed2 0, _) -> super#select_operation op args
      | (addr, arg) -> (Ispecific(Ilea addr), [Oper_val arg])
      end
  (* Recognize (x / cst) and (x % cst) only if cst is a power of 2. *)
  | Cdivi, [arg1; arg2] ->
    begin match arg2 with
    | Cconst_int n when self#is_immediate n && n = 1 lsl (Misc.log2 n) ->
      (Iintop(Idiv), [Oper_val arg1; Oper_mach (Oimm (Nativeint.of_int n))])
    | _ -> (Iintop(Idiv), [Oper_val arg1; Oper_val arg2]) end
  | Cmodi, [arg1; arg2] ->
    begin match arg2 with
    | Cconst_int n when self#is_immediate n && n = 1 lsl (Misc.log2 n) ->
      (Iintop(Imod), [Oper_val arg1; Oper_mach (Oimm (Nativeint.of_int n))])
    | _ -> (Iintop(Imod), [Oper_val arg1; Oper_val arg2]) end
  (* Recognize float arithmetic with memory. *)
  | Caddf, _ ->
      self#select_floatarith true Iaddf args
  | Csubf, _ ->
      self#select_floatarith false Isubf args
  | Cmulf, _ ->
      self#select_floatarith true Imulf args
  | Cdivf, _ ->
      self#select_floatarith false Idivf args
  | Cextcall("sqrt", _, false, _), 
    [Cop(Cload (Double|Double_u as chunk), [loc])] ->
         (Ispecific(Isqrtf), [Oper_addr(chunk, loc)])
  | Cextcall("sqrt", _, false, _), [arg] ->
      (Ispecific Isqrtf, [Oper_val arg])
  (* Recognize store instructions *)
  | Cstore Word, 
    [loc; Cop(Caddi, [Cop(Cload _, [loc']); Cconst_int n])]
      when loc = loc' && self#is_immediate n ->
      (Ispecific(Ioffset_loc(n)), [Oper_addr(Word, loc)])
  | _, _ -> super#select_operation op args

(* Recognize float arithmetic with mem *)

method select_floatarith commutative regular_op args =
  match args with
    [arg1; Cop(Cload (Double|Double_u as chunk), [loc2])] ->
      (regular_op, [Oper_val arg1; Oper_addr (chunk, loc2)])
  | [Cop(Cload (Double|Double_u as chunk), [loc1]); arg2] when commutative ->
      (regular_op, [Oper_val arg2; Oper_addr (chunk, loc1)])
  | [arg1; arg2] ->
      (regular_op, [Oper_val arg1; Oper_val arg2])
  | _ ->
      assert false

(* Deal with register constraints *)

method! insert_op_debug op dbg rs rd =
  try
    let (rsrc, rdst) = pseudoregs_for_operation op rs rd in
    self#insert_operand_moves rs rsrc;
    self#insert_debug (Iop op) dbg rsrc rdst;
    self#insert_moves rdst rd;
    rd
  with Use_default ->
    super#insert_op_debug op dbg rs rd

end

let fundecl f = (new selector)#emit_fundecl f
