(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Pretty-printing of pseudo machine code *)

open Format
open Cmm
open Reg
open Mach

let reg ppf r =
  if String.length r.name > 0 then
    fprintf ppf "%s" r.name
  else
    fprintf ppf "%s" (match r.typ with Addr -> "A" | Int -> "I" | Float -> "F");
  fprintf ppf "/%i" r.stamp;
  begin match r.loc with
  | Unknown -> ()
  | Reg r ->
      fprintf ppf "[%s]" (Proc.register_name r)
  | Stack(Local s) ->
      fprintf ppf "[s%i]" s
  | Stack(Incoming s) ->
      fprintf ppf "[si%i]" s
  | Stack(Outgoing s) ->
      fprintf ppf "[so%i]" s
  end

let operand ppf = function
  | Oreg r -> reg ppf r
  | Oimm n -> fprintf ppf "%s" (Nativeint.to_string n)
  | Omem(addr, regs) -> 
    fprintf ppf "[%a]" 
      (Arch.print_addressing reg addr) regs

let regs ppf v =
  match Array.length v with
  | 0 -> ()
  | 1 -> reg ppf v.(0)
  | n -> reg ppf v.(0);
         for i = 1 to n-1 do fprintf ppf " %a" reg v.(i) done

let operands ppf v =
  match Array.length v with
  | 0 -> ()
  | 1 -> operand ppf v.(0)
  | n -> operand ppf v.(0);
         for i = 1 to n-1 do fprintf ppf " %a" operand v.(i) done

let regset ppf s =
  let first = ref true in
  Reg.Set.iter
    (fun r ->
      if !first then begin first := false; fprintf ppf "%a" reg r end
      else fprintf ppf "@ %a" reg r)
    s

let regsetaddr ppf s =
  let first = ref true in
  Reg.Set.iter
    (fun r ->
      if !first then begin first := false; fprintf ppf "%a" reg r end
      else fprintf ppf "@ %a" reg r;
      match r.typ with Addr -> fprintf ppf "*" | _ -> ())
    s

let intcomp = function
  | Isigned c -> Printf.sprintf " %ss " (Printcmm.comparison c)
  | Iunsigned c -> Printf.sprintf " %su " (Printcmm.comparison c)

let floatcomp c =
    Printf.sprintf " %sf " (Printcmm.comparison c)

let intop = function
  | Iadd -> " + "
  | Isub -> " - "
  | Imul -> " * "
  | Idiv -> " div "
  | Imod -> " mod "
  | Iand -> " & "
  | Ior ->  " | "
  | Ixor -> " ^ "
  | Ilsl -> " << "
  | Ilsr -> " >>u "
  | Iasr -> " >>s "
  | Icomp cmp -> intcomp cmp
  | Icheckbound -> " check > "

let test tst ppf arg =
  match tst with
  | Itruetest -> operand ppf arg.(0)
  | Ifalsetest -> fprintf ppf "not %a" operand arg.(0)
  | Iinttest cmp -> fprintf ppf "%a%s%a" operand arg.(0) (intcomp cmp) operand arg.(1)
  | Ifloattest(cmp, neg) ->
      fprintf ppf "%s%a%s%a"
       (if neg then "not " else "")
        operand arg.(0) (floatcomp cmp) operand arg.(1)
  | Ieventest -> fprintf ppf "%a & 1 == 0" operand arg.(0)
  | Ioddtest -> fprintf ppf "%a & 1 == 1" operand arg.(0)

let print_live = ref false

let operation op arg ppf res =
  if Array.length res > 0 then fprintf ppf "%a := " regs res;
  match op with
  | Imove -> operands ppf arg
  | Ispill -> fprintf ppf "%a (spill)" operands arg
  | Ireload -> fprintf ppf "%a (reload)" operands arg
  | Iconst_int -> fprintf ppf "%a" operands arg
  | Iconst_float s -> fprintf ppf "%s" s
  | Iconst_symbol s -> fprintf ppf "\"%s\"" s
  | Icall_ind -> fprintf ppf "call %a" operands arg
  | Icall_imm lbl -> fprintf ppf "call \"%s\" %a" lbl operands arg
  | Itailcall_ind -> fprintf ppf "tailcall %a" operands arg
  | Itailcall_imm lbl -> fprintf ppf "tailcall \"%s\" %a" lbl operands arg
  | Iextcall(lbl, alloc) ->
      fprintf ppf "extcall \"%s\" %a%s" lbl operands arg
      (if not alloc then "" else " (noalloc)")
  | Istackoffset n ->
      fprintf ppf "offset stack %i" n
  | Iload chunk -> fprintf ppf "%s%a" (Printcmm.chunk chunk) operands arg
  | Istore chunk ->
      fprintf ppf "%s%a := %a" (Printcmm.chunk chunk) operand arg.(1) operand arg.(0)
  | Ialloc n -> fprintf ppf "alloc %i" n
  | Iintop(op) -> fprintf ppf "%a%s%a" operand arg.(0) (intop op) operand arg.(1)
  | Inegf -> fprintf ppf "-f %a" operand arg.(0)
  | Iabsf -> fprintf ppf "absf %a" operand arg.(0)
  | Iaddf -> fprintf ppf "%a +f %a" operand arg.(0) operand arg.(1)
  | Isubf -> fprintf ppf "%a -f %a" operand arg.(0) operand arg.(1)
  | Imulf -> fprintf ppf "%a *f %a" operand arg.(0) operand arg.(1)
  | Idivf -> fprintf ppf "%a /f %a" operand arg.(0) operand arg.(1)
  | Ifloatofint -> fprintf ppf "floatofint %a" operand arg.(0)
  | Iintoffloat -> fprintf ppf "intoffloat %a" operand arg.(0)
  | Ispecific op ->
      Arch.print_specific_operation operand op ppf arg

let rec instr ppf i =
  if !print_live then begin
    fprintf ppf "@[<1>{%a" regsetaddr i.live;
    let first = ref true in
    Mach.iter_operand_regs (fun r -> 
      fprintf ppf (if !first then (first := false; "@ +@ %a") else " %a") reg r) i.arg;
    fprintf ppf "}@]@,";
  end;
  begin match i.desc with
  | Iend -> ()
  | Iop op ->
      operation op i.arg ppf i.res
  | Ireturn ->
      fprintf ppf "return %a" operands i.arg
  | Iifthenelse(tst, ifso, ifnot) ->
      fprintf ppf "@[<v 2>if %a then@,%a" (test tst) i.arg instr ifso;
      begin match ifnot.desc with
      | Iend -> ()
      | _ -> fprintf ppf "@;<0 -2>else@,%a" instr ifnot
      end;
      fprintf ppf "@;<0 -2>endif@]"
  | Iswitch(index, cases) ->
      fprintf ppf "switch %a" operand i.arg.(0);
      for i = 0 to Array.length cases - 1 do
        fprintf ppf "@,@[<v 2>@[";
        for j = 0 to Array.length index - 1 do
          if index.(j) = i then fprintf ppf "case %i:@," j
        done;
        fprintf ppf "@]@,%a@]" instr cases.(i)
      done;
      fprintf ppf "@,endswitch"
  | Iloop(body) ->
      fprintf ppf "@[<v 2>loop@,%a@;<0 -2>endloop@]" instr body
  | Icatch(i, body, handler) ->
      fprintf
        ppf "@[<v 2>catch@,%a@;<0 -2>with(%d)@,%a@;<0 -2>endcatch@]"
        instr body i instr handler
  | Iexit i ->
      fprintf ppf "exit(%d)" i
  | Itrywith(body, handler) ->
      fprintf ppf "@[<v 2>try@,%a@;<0 -2>with@,%a@;<0 -2>endtry@]"
             instr body instr handler
  | Iraise ->
      fprintf ppf "raise %a" operand i.arg.(0)
  end;
  if not (Debuginfo.is_none i.dbg) then
    fprintf ppf "%s" (Debuginfo.to_string i.dbg);
  begin match i.next.desc with
    Iend -> ()
  | _ -> fprintf ppf "@,%a" instr i.next
  end

let fundecl ppf f =
  let dbg =
    if Debuginfo.is_none f.fun_dbg then
      ""
    else
      " " ^ Debuginfo.to_string f.fun_dbg in
  fprintf ppf "@[<v 2>%s(%a)%s@,%a@]"
    f.fun_name regs f.fun_args dbg instr f.fun_body

let phase msg ppf f =
  fprintf ppf "*** %s@.%a@." msg fundecl f

let interference ppf r =
  let interf ppf =
   List.iter
    (fun r -> fprintf ppf "@ %a" reg r)
    r.interf in
  fprintf ppf "@[<2>%a:%t@]@." reg r interf

let interferences ppf () =
  fprintf ppf "*** Interferences@.";
  List.iter (interference ppf) (Reg.all_registers())

let preference ppf r =
  let prefs ppf =
    List.iter
      (fun (r, w) -> fprintf ppf "@ %a weight %i" reg r w)
      r.prefer in
  fprintf ppf "@[<2>%a: %t@]@." reg r prefs

let preferences ppf () =
  fprintf ppf "*** Preferences@.";
  List.iter (preference ppf) (Reg.all_registers())
