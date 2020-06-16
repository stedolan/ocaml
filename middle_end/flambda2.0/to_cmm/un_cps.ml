(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

[@@@ocaml.warning "-32"] (* FIXME Let_code -- just remove this *)

open! Flambda.Import

module Env = Un_cps_env
module Ece = Effects_and_coeffects
module R = Un_cps_result

(* Notes:
   - an int64 on a 32-bit host is represented across two registers,
     hence most operations on them will actually need to call C primitive
     that can handle them.
   - int32 on 64 bits are represented as an int64 in the range of
     32-bit integers. Thus we insert sign extensions after every operation
     on 32-bits integers that may have a result outside of the range.
*)

(* TODO: remove all uses of this, ^^ *)
let todo () = failwith "Not yet implemented"

(* Cmm helpers *)
module C = struct
  include Cmm_helpers
  include Un_cps_helper
end

(* Type for the result of translating a set of closures *)
type set_of_closure_result = {
  cmm : Cmm.expression;
  (* The cmm expression whose return value is the set of closure. *)
  env : Env.t;
  (* The env that result from the translation of the closure *)
  effs : Ece.t;
  (* The effects produced by evaluating the {cmm} expression. *)
  res : R.t;
  (* The static result (i.e. statically allocated constants, etc..) after
     translating the closure. *)
}


(* Shortcuts for useful cmm machtypes *)
let typ_int = Cmm.typ_int
let typ_val = Cmm.typ_val
let typ_float = Cmm.typ_float
let typ_int64 = C.typ_int64

(* CR gbury: this conversion is potentially unsafe when cross-compiling
   for a 64-bit machine on a 32-bit host *)
let nativeint_of_targetint t =
  match Targetint.repr t with
  | Int32 i -> Nativeint.of_int32 i
  | Int64 i -> Int64.to_nativeint i

(* CR gbury: {Targetint.to_int} should raise an error when converting
   an out-of-range integer. *)
let int_of_targetint t =
  let i = Targetint.to_int t in
  let t' = Targetint.of_int i in
  if not (Targetint.equal t t') then
    Misc.fatal_errorf "Cannot translate targetint to caml int";
  i


(* Name expressions *)

let symbol s =
  Linkage_name.to_string (Symbol.linkage_name s)

let name env name =
  Name.pattern_match name
    ~var:(fun v -> Env.inline_variable env v)
    ~symbol:(fun s ->
      let env =
        Env.check_scope ~allow_deleted:false env (Code_id_or_symbol.Symbol s)
      in
      C.symbol (symbol s), env, Ece.pure)

(* Constants *)

let tag_targetint t = Targetint.(add (shift_left t 1) one)
let targetint_of_imm i = Targetint.OCaml.to_targetint i.Target_imm.value

let const _env cst =
  match Reg_width_const.descr cst with
  | Naked_immediate i ->
    C.targetint (targetint_of_imm i)
  | Tagged_immediate i ->
    C.targetint (tag_targetint (targetint_of_imm i))
  | Naked_float f ->
    C.float (Numbers.Float_by_bit_pattern.to_float f)
  | Naked_int32 i -> C.int32 i
  | Naked_int64 i -> C.int64 i
  | Naked_nativeint t -> C.targetint t

let default_of_kind (k : Flambda_kind.t) =
  match k with
  | Value -> C.int 1
  | Naked_number Naked_immediate -> C.int 0
  | Naked_number Naked_float -> C.float 0.
  | Naked_number Naked_int32 -> C.int 0
  | Naked_number Naked_int64 when C.arch32 -> todo ()
  | Naked_number Naked_int64 -> C.int 0
  | Naked_number Naked_nativeint -> C.int 0
  | Fabricated -> Misc.fatal_error "Fabricated_kind have no default value"

(* Function symbol *)

let function_name simple =
  let fail simple =
    Misc.fatal_errorf
      "Expected a function symbol, instead of@ %a" Simple.print simple
  in
  Simple.pattern_match simple
    ~name:(fun name ->
      Name.pattern_match name
        ~var:(fun _ -> fail simple)
        ~symbol:(fun sym -> symbol sym))
    ~const:(fun _ -> fail simple)

(* 'Simple' expression *)

let simple env s =
  Simple.pattern_match s
    ~name:(fun n -> name env n)
    ~const:(fun c -> const env c, env, Ece.pure)

(* Arithmetic primitives *)

let primitive_boxed_int_of_standard_int x =
  match (x : Flambda_kind.Standard_int.t) with
  | Naked_int32 -> Primitive.Pint32
  | Naked_int64 -> Primitive.Pint64
  | Naked_nativeint -> Primitive.Pnativeint
  | Tagged_immediate -> assert false
  | Naked_immediate -> assert false

let unary_int_arith_primitive _env dbg kind op arg =
  match (kind : Flambda_kind.Standard_int.t),
        (op : Flambda_primitive.unary_int_arith_op) with
  | Tagged_immediate, Neg -> C.negint arg dbg
  | Tagged_immediate, Swap_byte_endianness ->
    (* CR mshinwell for gbury: This could maybe cause a fatal error now? *)
    let untagged = C.untag_int arg dbg in
    let swapped = C.bswap16 untagged dbg in
    C.tag_int swapped dbg
  | Naked_immediate, Swap_byte_endianness -> C.bswap16 arg dbg
  (* Special case for manipulating int64 on 32-bit hosts *)
  | Naked_int64, Neg when C.arch32 ->
    C.extcall ~alloc:false "caml_int64_neg_native" typ_int64 [arg]
  (* General case (including byte swap for 64-bit on 32-bit archi) *)
  | _, Neg -> C.sub_int (C.int 0) arg dbg
  | _, Swap_byte_endianness ->
    let primitive_kind = primitive_boxed_int_of_standard_int kind in
    C.bbswap primitive_kind arg dbg

let unary_float_arith_primitive _env dbg op arg =
  match (op : Flambda_primitive.unary_float_arith_op) with
  | Abs -> C.float_abs ~dbg arg
  | Neg -> C.float_neg ~dbg arg

let arithmetic_conversion dbg src dst arg =
  let open Flambda_kind.Standard_int_or_float in
  match src, dst with
  (* 64-bit on 32-bit host specific cases *)
  | Naked_int64, Tagged_immediate when C.arch32 ->
    None, C.extcall ~alloc:false "caml_int64_to_int" typ_int [arg]
  | Naked_int64, Naked_int32 when C.arch32 ->
    None, C.extcall ~alloc:false "caml_int64_to_int32" typ_int [arg]
  | Naked_int64, (Naked_nativeint | Naked_immediate) when C.arch32 ->
    None, C.extcall ~alloc:false "caml_int64_to_nativeint" typ_int [arg]
  | Naked_int64, Naked_float when C.arch32 ->
    None, C.extcall ~alloc:false "caml_int64_to_float_unboxed" typ_float [arg]
  | Tagged_immediate, Naked_int64 when C.arch32 ->
    None, C.extcall ~alloc:true "caml_int64_of_int" typ_val [arg]
          |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  | Naked_int32, Naked_int64 when C.arch32 ->
    None, C.extcall ~alloc:true "caml_int64_of_int32" typ_val [arg]
          |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  | (Naked_nativeint | Naked_immediate), Naked_int64 when C.arch32 ->
    None, C.extcall ~alloc:true "caml_int64_of_nativeint" typ_val [arg]
          |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  | Naked_float, Naked_int64 when C.arch32 ->
    None, C.extcall ~alloc:true "caml_int64_of_float_unboxed" typ_val [arg]
          |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  (* Identity on floats *)
  | Naked_float, Naked_float -> None, arg
  (* Conversions to and from tagged ints  *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate),
    Tagged_immediate ->
    None, C.tag_int arg dbg
  | Tagged_immediate, (Naked_int64 | Naked_nativeint | Naked_immediate) ->
    Some (Env.Untag arg), C.untag_int arg dbg
  (* Operations resulting in int32s must take care to sign_extend the res *)
  | Tagged_immediate, Naked_int32 ->
    None, C.sign_extend_32 dbg (C.untag_int arg dbg)
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate),
    Naked_int32 ->
    None, C.sign_extend_32 dbg arg
  (* No-op conversions *)
  | Tagged_immediate, Tagged_immediate
  | Naked_int32, (Naked_int64 | Naked_nativeint | Naked_immediate)
  | Naked_int64, (Naked_int64 | Naked_nativeint | Naked_immediate)
  | Naked_nativeint, (Naked_int64 | Naked_nativeint | Naked_immediate)
  | Naked_immediate, (Naked_int64 | Naked_nativeint | Naked_immediate) ->
    None, arg
  (* Int-Float conversions *)
  | Tagged_immediate, Naked_float ->
    None, C.float_of_int ~dbg (C.untag_int arg dbg)
  | (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint),
    Naked_float ->
    None, C.float_of_int ~dbg arg
  | Naked_float, Tagged_immediate ->
    None, C.tag_int (C.int_of_float ~dbg arg) dbg
  | Naked_float, (Naked_immediate | Naked_int64 | Naked_nativeint) ->
    None, C.int_of_float ~dbg arg
  | Naked_float, Naked_int32 ->
    None, C.sign_extend_32 dbg (C.int_of_float ~dbg arg)

let binary_phys_comparison _env dbg kind op x y =
  match (kind : Flambda_kind.t),
        (op : Flambda_primitive.equality_comparison) with
  (* int64 special case *)
  | Naked_number Naked_int64, Eq when C.arch32 ->
    C.untag_int
      (C.extcall ~alloc:true "caml_equal" typ_int
         [C.box_int64 ~dbg x; C.box_int64 ~dbg y])
      dbg
  | Naked_number Naked_int64, Neq when C.arch32 ->
    C.untag_int
      (C.extcall ~alloc:true "caml_notequal" typ_int
         [C.box_int64 ~dbg x; C.box_int64 ~dbg y])
      dbg
  (* General case *)
  | _, Eq -> C.eq ~dbg x y
  | _, Neq -> C.neq ~dbg x y

let binary_int_arith_primitive _env dbg kind op x y =
  match (kind : Flambda_kind.Standard_int.t),
        (op : Flambda_primitive.binary_int_arith_op) with
  (* Int64 bits ints on 32-bit archs *)
  | Naked_int64, Add when C.arch32 ->
    C.extcall ~alloc:false "caml_int64_add_native" typ_int64 [x; y]
  | Naked_int64, Sub when C.arch32 ->
    C.extcall ~alloc:false "caml_int64_sub_native" typ_int64 [x; y]
  | Naked_int64, Mul when C.arch32 ->
    C.extcall ~alloc:false "caml_int64_mul_native" typ_int64 [x; y]
  | Naked_int64, Div when C.arch32 ->
    C.extcall ~alloc:true "caml_int64_div_native" typ_int64 [x; y]
  | Naked_int64, Mod when C.arch32 ->
    C.extcall ~alloc:true "caml_int64_mod_native" typ_int64 [x; y]
  | Naked_int64, And when C.arch32 ->
    C.extcall ~alloc:false "caml_int64_and_native" typ_int64 [x; y]
  | Naked_int64, Or when C.arch32 ->
    C.extcall ~alloc:false "caml_int64_or_native" typ_int64 [x; y]
  | Naked_int64, Xor when C.arch32 ->
    C.extcall ~alloc:false "caml_int64_xor_native" typ_int64 [x; y]
  (* Tagged integers *)
  | Tagged_immediate, Add -> C.add_int_caml x y dbg
  | Tagged_immediate, Sub -> C.sub_int_caml x y dbg
  | Tagged_immediate, Mul -> C.mul_int_caml x y dbg
  | Tagged_immediate, Div -> C.div_int_caml Lambda.Unsafe x y dbg
  | Tagged_immediate, Mod -> C.mod_int_caml Lambda.Unsafe x y dbg
  | Tagged_immediate, And -> C.and_int_caml x y dbg
  | Tagged_immediate, Or  -> C.or_int_caml x y dbg
  | Tagged_immediate, Xor -> C.xor_int_caml x y dbg
  (* Operations on 32-bits integers arguments must return something in
     the range of 32-bits integers, hence the sign_extensions here *)
  | Naked_int32, Add ->
    C.sign_extend_32 dbg (C.add_int (C.low_32 dbg x) (C.low_32 dbg y) dbg)
  | Naked_int32, Sub ->
    C.sign_extend_32 dbg (C.sub_int (C.low_32 dbg x) (C.low_32 dbg y) dbg)
  | Naked_int32, Mul ->
    C.sign_extend_32 dbg (C.mul_int (C.low_32 dbg x) (C.low_32 dbg y) dbg)
  | Naked_int32, Xor ->
    C.sign_extend_32 dbg (C.xor_ ~dbg (C.low_32 dbg x) (C.low_32 dbg y))
  | Naked_int32, And ->
    C.sign_extend_32 dbg (C.and_ ~dbg (C.low_32 dbg x) (C.low_32 dbg y))
  | Naked_int32, Or ->
    C.sign_extend_32 dbg (C.or_ ~dbg (C.low_32 dbg x) (C.low_32 dbg y))
  (* Naked ints *)
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Add ->
    C.add_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Sub ->
    C.sub_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Mul ->
    C.mul_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), And ->
    C.and_ ~dbg x y
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Or ->
    C.or_ ~dbg x y
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Xor ->
    C.xor_ ~dbg x y
  (* Division and modulo need some extra care *)
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Div ->
    let bi = C.primitive_boxed_int_of_standard_int kind in
    C.safe_div_bi Lambda.Unsafe x y bi dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Mod ->
    let bi = C.primitive_boxed_int_of_standard_int kind in
    C.safe_mod_bi Lambda.Unsafe x y bi dbg
  | Naked_int32, Div ->
    let bi = C.primitive_boxed_int_of_standard_int kind in
    C.sign_extend_32 dbg (C.safe_div_bi Lambda.Unsafe x y bi dbg)
  | Naked_int32, Mod ->
    let bi = C.primitive_boxed_int_of_standard_int kind in
    C.sign_extend_32 dbg (C.safe_mod_bi Lambda.Unsafe x y bi dbg)

let binary_int_shift_primitive _env dbg kind op x y =
  match (kind : Flambda_kind.Standard_int.t),
        (op : Flambda_primitive.int_shift_op) with
  (* Int64 special case *)
  | Naked_int64, Lsl when C.arch32 ->
    todo() (* caml primitives for these have no native/unboxed version *)
  | Naked_int64, Lsr when C.arch32 ->
    todo() (* caml primitives for these have no native/unboxed version *)
  | Naked_int64, Asr when C.arch32 ->
    todo() (* caml primitives for these have no native/unboxed version *)
  (* Tagged integers *)
  | Tagged_immediate, Lsl -> C.lsl_int_caml_raw ~dbg x y
  | Tagged_immediate, Lsr -> C.lsr_int_caml_raw ~dbg x y
  | Tagged_immediate, Asr -> C.asr_int_caml_raw ~dbg x y
  (* Operations on 32-bits integers need to ensure their result are within
     the 32-bit range, hence the sign_extension. *)
  | Naked_int32, Lsl ->
    C.sign_extend_32 dbg (C.lsl_int (C.low_32 dbg x) y dbg)
  | Naked_int32, Lsr ->
    let arg = if C.arch64 then C.zero_extend_32 dbg x else x in
    C.sign_extend_32 dbg (C.lsr_int arg y dbg)
  | Naked_int32, Asr ->
    C.sign_extend_32 dbg (C.asr_int x y dbg)
  (* Naked ints *)
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Lsl ->
    C.lsl_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Lsr ->
    C.lsr_int x y dbg
  | (Naked_int64 | Naked_nativeint | Naked_immediate), Asr ->
    C.asr_int x y dbg

let binary_int_comp_primitive _env dbg kind signed cmp x y =
  match (kind : Flambda_kind.Standard_int.t),
        (signed : Flambda_primitive.signed_or_unsigned),
        (cmp : Flambda_primitive.ordered_comparison) with
  (* XXX arch32 cases need [untag_int] now. *)
  | Naked_int64, Signed, Lt when C.arch32 ->
    C.extcall ~alloc:true "caml_lessthan" typ_int
      [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  | Naked_int64, Signed, Le when C.arch32 ->
    C.extcall ~alloc:true "caml_lessequal" typ_int
      [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  | Naked_int64, Signed, Gt when C.arch32 ->
    C.extcall ~alloc:true "caml_greaterthan" typ_int
      [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  | Naked_int64, Signed, Ge when C.arch32 ->
    C.extcall ~alloc:true "caml_greaterequal" typ_int
      [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  | Naked_int64, Unsigned, (Lt | Le | Gt | Ge) when C.arch32 ->
    todo() (* There are no runtime C functions to do that afaict *)
  (* Tagged integers *)
  (* When comparing tagged integers, there is always one number for which
     the last bit is irrelevant.
     For x < y, ignoring the last bit of y will not change the result, as
     if x and y are different (as OCaml integers) then the comparison doesn't
     need to see the last bit, and if they are equal then if the last bit of
     x is one (as it is supposed to be) the result will be false
     for both values of the last bit of y, as expected.
     The same reasoning applies to the other comparisons. *)
  | Tagged_immediate, Signed, Lt -> C.lt ~dbg x (C.ignore_low_bit_int y)
  | Tagged_immediate, Signed, Le -> C.le ~dbg (C.ignore_low_bit_int x) y
  | Tagged_immediate, Signed, Gt -> C.gt ~dbg (C.ignore_low_bit_int x) y
  | Tagged_immediate, Signed, Ge -> C.ge ~dbg x (C.ignore_low_bit_int y)
  | Tagged_immediate, Unsigned, Lt -> C.ult ~dbg x (C.ignore_low_bit_int y)
  | Tagged_immediate, Unsigned, Le -> C.ule ~dbg (C.ignore_low_bit_int x) y
  | Tagged_immediate, Unsigned, Gt -> C.ugt ~dbg (C.ignore_low_bit_int x) y
  | Tagged_immediate, Unsigned, Ge -> C.uge ~dbg x (C.ignore_low_bit_int y)
  (* Naked integers. *)
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Signed, Lt ->
    C.lt ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Signed, Le ->
    C.le ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Signed, Gt ->
    C.gt ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Signed, Ge ->
    C.ge ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Unsigned, Lt ->
    C.ult ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Unsigned, Le ->
    C.ule ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Unsigned, Gt ->
    C.ugt ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Unsigned, Ge ->
    C.uge ~dbg x y

let binary_float_arith_primitive _env dbg op x y =
  match (op : Flambda_primitive.binary_float_arith_op) with
  | Add -> C.float_add ~dbg x y
  | Sub -> C.float_sub ~dbg x y
  | Mul -> C.float_mul ~dbg x y
  | Div -> C.float_div ~dbg x y

let binary_float_comp_primitive _env dbg op x y =
  match (op : Flambda_primitive.comparison) with
  | Eq -> C.float_eq ~dbg x y
  | Neq -> C.float_neq ~dbg x y
  | Lt -> C.float_lt ~dbg x y
  | Gt -> C.float_gt ~dbg x y
  | Le -> C.float_le ~dbg x y
  | Ge -> C.float_ge ~dbg x y

(* Primitives *)

let unary_primitive env dbg f arg =
  match (f : Flambda_primitive.unary_primitive) with
  | Duplicate_array _ ->
    None, C.extcall ~alloc:true "caml_obj_dup" typ_val [arg]
  | Duplicate_block _ ->
    None, C.extcall ~alloc:true "caml_obj_dup" typ_val [arg]
  | Is_int ->
    None, C.and_ ~dbg arg (C.int ~dbg 1)
  | Get_tag ->
    None, C.get_tag arg dbg
  | Array_length array_kind ->
    None, C.array_length ~dbg array_kind arg
  | Bigarray_length { dimension } ->
    None, C.load ~dbg Cmm.Word_int Asttypes.Mutable
            (C.field_address arg (4 + dimension) dbg)
  | String_length _ ->
    None, C.string_length arg dbg
  | Int_as_pointer ->
    None, C.int_as_pointer arg dbg
  | Opaque_identity ->
    None, arg
  | Int_arith (kind, op) ->
    None, unary_int_arith_primitive env dbg kind op arg
  | Float_arith op ->
    None, unary_float_arith_primitive env dbg op arg
  | Num_conv { src; dst; } ->
    arithmetic_conversion dbg src dst arg
  | Boolean_not ->
    None, C.mk_not dbg arg
  | Unbox_number kind ->
    let extra =
      match kind with
      | Untagged_immediate -> Some (Env.Untag arg)
      | _ -> None
    in
    extra, C.unbox_number ~dbg kind arg
  | Box_number kind ->
    None, C.box_number ~dbg kind arg
  | Select_closure { move_from = c1; move_to = c2} ->
    begin match Env.closure_offset env c1, Env.closure_offset env c2 with
    | Some { offset = c1_offset; _ }, Some { offset = c2_offset; _ } ->
      let diff = c2_offset - c1_offset in
      None, C.infix_field_address ~dbg arg diff
    | Some _, None | None, Some _ | None, None ->
      None, C.unreachable
    end
  | Project_var { project_from; var; } ->
    match Env.env_var_offset env var, Env.closure_offset env project_from with
    | Some { offset; }, Some { offset = base; _ } ->
      None, C.get_field_gen Asttypes.Immutable arg (offset - base) dbg
    | Some _, None | None, Some _ | None, None ->
      None, C.unreachable

let binary_primitive env dbg f x y =
  match (f : Flambda_primitive.binary_primitive) with
  | Block_load (kind, mut) ->
    C.block_load ~dbg kind mut x y
  | Array_load (kind, _mut) ->
    C.array_load ~dbg kind x y
  | String_or_bigstring_load (kind, width) ->
    C.string_like_load ~dbg kind width x y
  | Bigarray_load (dimensions, kind, layout) ->
    C.bigarray_load ~dbg dimensions kind layout x y
  | Phys_equal (kind, op) ->
    binary_phys_comparison env dbg kind op x y
  | Int_arith (kind, op) ->
    binary_int_arith_primitive env dbg kind op x y
  | Int_shift (kind, op) ->
    binary_int_shift_primitive env dbg kind op x y
  | Int_comp (kind, signed, cmp) ->
    binary_int_comp_primitive env dbg kind signed cmp x y
  | Float_arith op ->
    binary_float_arith_primitive env dbg op x y
  | Float_comp cmp ->
    binary_float_comp_primitive env dbg cmp x y

let ternary_primitive _env dbg f x y z =
  match (f : Flambda_primitive.ternary_primitive) with
  | Block_set (block_access, init) ->
    C.block_set ~dbg block_access init x y z
  | Array_set (array_kind, init) ->
    C.array_set ~dbg array_kind init x y z
  | Bytes_or_bigstring_set (kind, width) ->
    C.bytes_like_set ~dbg kind width x y z
  | Bigarray_set (dimensions, kind, layout) ->
    C.bigarray_store ~dbg dimensions kind layout x y z

let variadic_primitive _env dbg f args =
  match (f : Flambda_primitive.variadic_primitive) with
  | Make_block (kind, _mut) ->
    C.make_block ~dbg kind args
  | Make_array (kind, _mut) ->
    C.make_array ~dbg kind args

let arg_list env l =
  let aux (list, env, effs) x =
    let y, env, eff = simple env x in
    (y :: list, env, Ece.join eff effs)
  in
  let args, env, effs = List.fold_left aux ([], env, Ece.pure) l in
  List.rev args, env, effs

(* CR Gbury: check the order in which the primitive arguments are
   given to [Env.inline_variable]. *)
let prim env dbg p =
  match (p : Flambda_primitive.t) with
  | Unary (f, x) ->
    let x, env, eff = simple env x in
    let extra, res = unary_primitive env dbg f x in
    res, extra, env, eff
  | Binary (f, x, y) ->
    let x, env, effx = simple env x in
    let y, env, effy = simple env y in
    let effs = Ece.join effx effy in
    let res = binary_primitive env dbg f x y in
    res, None, env, effs
  | Ternary (f, x, y, z) ->
    let x, env, effx = simple env x in
    let y, env, effy = simple env y in
    let z, env, effz = simple env z in
    let effs = Ece.join (Ece.join effx effy) effz in
    let res = ternary_primitive env dbg f x y z in
    res, None, env, effs
  | Variadic (f, l) ->
    let args, env, effs = arg_list env l in
    let res = variadic_primitive env dbg f args in
    res, None, env, effs

(* Kinds and types *)

let machtype_of_kind k =
  match (k  : Flambda_kind.t) with
  | Value -> typ_val
  | Naked_number Naked_float -> typ_float
  | Naked_number Naked_int64 -> typ_int64
  | Naked_number (Naked_immediate | Naked_int32 | Naked_nativeint) ->
    typ_int
  | Fabricated -> assert false

let machtype_of_kinded_parameter p =
  machtype_of_kind (Kinded_parameter.kind p)

let machtype_of_return_arity = function
  | [k] -> machtype_of_kind k
  | _ -> (* TODO: update when unboxed tuples are used *)
    Misc.fatal_errorf
      "Functions are currently limited to a single return value"

let meth_kind k =
  match (k : Call_kind.method_kind) with
  | Self -> (Self : Lambda.meth_kind)
  | Public -> (Public : Lambda.meth_kind)
  | Cached -> (Cached : Lambda.meth_kind)

let wrap_extcall_result (l : Flambda_kind.t list) =
  match l with
  | [Naked_number Naked_int32] -> C.sign_extend_32
  | [_] -> (fun _dbg cmm -> cmm)
  | _ -> (* TODO: update when unboxed tuples are used *)
    Misc.fatal_errorf
      "Functions are currently limited to a single return value"

(* Closure variables *)

let filter_closure_vars env s =
  let used_closure_vars = Env.used_closure_vars env in
  let aux clos_var _bound_to =
    Var_within_closure.Set.mem clos_var used_closure_vars
  in
  Var_within_closure.Map.filter aux s


(* Function calls and continuations *)

let var_list env l =
  let flambda_vars = List.map Kinded_parameter.var l in
  let env, cmm_vars = Env.create_variables env flambda_vars in
  let vars = List.map2 (fun v v' ->
    (v, machtype_of_kinded_parameter v')
  ) cmm_vars l in
  env, vars

let split_exn_cont_args k = function
  | (v, _) :: rest ->
    v, rest
  | [] ->
    Misc.fatal_errorf
      "Exception continuation %a should have at least one argument"
      Continuation.print k

(* effects and co-effects *)

let cont_has_one_occurrence k num =
  match (num : Name_occurrences.Num_occurrences.t) with
  | One -> true
  | More_than_one -> false
  | Zero ->
    Misc.fatal_errorf
      "Found unused let-bound continuation %a, this should not happen"
      Continuation.print k

type inlining_decision =
  | Skip (* no use, the bound variable can be skipped/ignored *)
  | Inline (* the variable is used once, we can try and inline its use *)
  | Regular (* the variable is used multiple times, do not try and inline it. *)

let decide_inline_let effs v body =
  let free_names = Expr.free_names body in
  match Name_occurrences.count_variable free_names v with
  | Zero ->
    if Effects_and_coeffects.has_commuting_effects effs
    then Regular (* Could be Inline technically, but it's not clear the
                    code would be better (nor more readable). *)
    else Skip
  | One -> Inline
  | More_than_one -> Regular

(* Helpers for translating functions *)

let is_var_used v e =
  let free_names = Expr.free_names e in
  Name_occurrences.mem_var free_names v

let function_args vars my_closure body =
  if is_var_used my_closure body then begin
    let last_arg = Kinded_parameter.create my_closure Flambda_kind.value in
    vars @ [last_arg]
  end else
    vars

let function_flags () =
  if !Clflags.optimize_for_speed then
    []
  else
    [ Cmm.Reduce_code_size ]

(* Expressions *)

let rec expr env res e =
  match (Expr.descr e : Expr.descr) with
  | Let e' -> let_expr env res e'
  | Let_symbol e' -> let_symbol env res e'
  | Let_cont e' -> let_cont env res e'
  | Apply e' -> apply_expr env res e'
  | Apply_cont e' -> apply_cont env res e'
  | Switch e' -> switch env res e'
  | Invalid e' -> invalid env res e'

and named env res n =
  match (n : Named.t) with
  | Simple s ->
    let t, env, effs = simple env s in
    t, None, env, effs, res
  | Set_of_closures s ->
    let { cmm; env; effs; res; } = set_of_closures env res s in
    cmm, None, env, effs, res
  | Prim (p, dbg) ->
    let prim_eff = Flambda_primitive.effects_and_coeffects p in
    let t, extra, env, effs = prim env dbg p in
    t, extra, env, Ece.join effs prim_eff, res

and let_expr env res t =
  Let.pattern_match t ~f:(fun ~bound_vars ~body ->
    let mode = Bindable_let_bound.name_mode bound_vars in
    begin match Name_mode.descr mode with
    | In_types ->
      Misc.fatal_errorf
        "Binding in terms a variable of mode In_types is forbidden"
    | Phantom ->
      expr env res body
    | Normal ->
      let e = Let.defining_expr t in
      begin match bound_vars, e with
      | Singleton v, _ ->
        let v = Var_in_binding_pos.var v in
        let_expr_aux env res v e body
      | Set_of_closures { closure_vars; _ }, Set_of_closures soc ->
        let_set_of_closures env res body closure_vars soc
      | Set_of_closures _, (Simple _ | Prim _) ->
        Misc.fatal_errorf
          "Set_of_closures binding a non-Set_of_closures:@ %a"
          Let.print t
      end
    end)

and let_symbol env res let_sym =
  let body = Let_symbol.body let_sym in
  let bound_symbols = Let_symbol.bound_symbols let_sym in
  let env =
    (* All bound symbols are allowed to appear in each other's definition,
       so they're added to the environment first *)
    Env.add_to_scope env
      (Let_symbol.Bound_symbols.everything_being_defined bound_symbols)
  in
  let env, res, update_opt =
    Un_cps_static.static_const
      env res ~params_and_body
      (Let_symbol.bound_symbols let_sym)
      (Let_symbol.defining_expr let_sym)
  in
  match update_opt with
  | None -> expr env res body (* trying to preserve tail calls whenever we can *)
  | Some update ->
    let wrap, env = Env.flush_delayed_lets env in
    let body, res = expr env res body in
    wrap (C.sequence update body), res

and let_set_of_closures env res body closure_vars soc =
  (* First translate the set of closures, and bind it in the env *)
  let { cmm = csoc; env; effs; res; } = set_of_closures env res soc in
  let soc_var = Variable.create "*set_of_closures*" in
  (* CR gbury: allow inlining of the variable when possible *)
  let env = Env.bind_variable env soc_var effs false csoc in
  (* Then get from the env the cmm variable that was created and bound
     to the compiled set of closures. *)
  let soc_cmm_var, env, peff = Env.inline_variable env soc_var in
  assert (Ece.is_pure peff);
  (* Add env bindings for all the closure variables. *)
  let effs = Effects_and_coeffects.read in
  let env =
    Closure_id.Map.fold (fun cid v acc ->
      let v = Var_in_binding_pos.var v in
      match Env.closure_offset env cid with
      | Some { offset; _ } ->
        let e =
          C.infix_field_address ~dbg: Debuginfo.none
            soc_cmm_var offset
        in
        let_expr_bind body acc v e effs
      | None ->
        Misc.fatal_errorf "No closure offset for %a" Closure_id.print cid
    ) closure_vars env in
  (* The set of closures, as well as the individual closures variables
     are correctly set in the env, go on translating the body. *)
  expr env res body

and let_expr_bind ?extra body env v cmm_expr effs =
  match decide_inline_let effs v body with
  | Skip -> env
  | Inline -> Env.bind_variable env v ?extra effs true cmm_expr
  | Regular -> Env.bind_variable env v ?extra effs false cmm_expr

and let_expr_env body (env, res) v e =
  let cmm_expr, extra, env, effs, res = named env res e in
  let_expr_bind ?extra body env v cmm_expr effs, res

and let_expr_aux env res v e body =
  let env, res = let_expr_env body (env, res) v e in
  expr env res body

and decide_inline_cont h k num_free_occurrences =
  not (Continuation_handler.is_exn_handler h)
  && (Continuation_handler.stub h
      || cont_has_one_occurrence k num_free_occurrences)

and let_cont env res = function
  | Let_cont.Non_recursive { handler; num_free_occurrences; } ->
    Non_recursive_let_cont_handler.pattern_match handler ~f:(fun k ~body ->
      let h = Non_recursive_let_cont_handler.handler handler in
      if decide_inline_cont h k num_free_occurrences then begin
        let_cont_inline env res k h body
      end else
        let_cont_jump env res k h body
    )
  | Let_cont.Recursive handlers ->
    Recursive_let_cont_handlers.pattern_match handlers ~f:(fun ~body conts ->
      assert (not (Continuation_handlers.contains_exn_handler conts));
      let_cont_rec env res conts body
    )

(* The bound continuation [k] will be inlined. *)
and let_cont_inline env res k h body =
  let args, handler = continuation_handler_split h in
  let env = Env.add_inline_cont env k args handler in
  expr env res body

(* Continuations that are not inlined are translated using a jump:
   - exceptions continuations use "dynamic" jumps using the
     raise/trywith cmm mechanism
   - regular continuations use static jumps, through the
     exit/catch cmm mechanism *)
(* CR Gbury: "split" the environment according to which variables the
   handler and the body uses, to allow for inlining to proceed
   within each expression. *)
and let_cont_jump env res k h body =
  let wrap, env = Env.flush_delayed_lets env in
  let vars, handle, res = continuation_handler env res h in
  let id, env = Env.add_jump_cont env (List.map snd vars) k in
  if Continuation_handler.is_exn_handler h then begin
    let body, res = let_cont_exn env res k h body vars handle id in
    wrap body, res
  end else begin
    let body, res = expr env res body in
    wrap (C.ccatch
            ~rec_flag:false ~body
            ~handlers:[C.handler id vars handle]
         ), res
  end

(* Exception continuations, translated using delayed trywith blocks.
   Additionally, exn continuations in flambda2 can have extra args, which
   are passed through the trywith using mutable cmm variables. Thus the
   exn handler must first read the contents of thos extra args (eagerly
   in order to minmize the lifetime of the mutable variables) *)
and let_cont_exn env res k h body vars handle id =
  let exn_var, extra_params = split_exn_cont_args k vars in
  let env_body, extra_vars = Env.add_exn_handler env k h in
  let handler = exn_handler handle extra_vars extra_params in
  let body, res = expr env_body res body in
  let trywith =
    C.trywith
      ~dbg:Debuginfo.none
      ~kind:(Delayed id)
      ~body ~exn_var ~handler
  in
  wrap_let_cont_exn_body trywith extra_vars, res

(* wrap a exn handler with read of the mutable variables *)
and exn_handler handle extra_vars extra_params =
  List.fold_left2
    (fun handler (v, _) (p, _) -> C.letin p (C.var v) handler)
    handle extra_vars extra_params

(* define and initialize the mutable cmm variables used by an exn extra args *)
and wrap_let_cont_exn_body handler extra_vars =
  List.fold_left (fun expr (v, k) ->
    let v = Backend_var.With_provenance.create v in
    C.letin_mut v (machtype_of_kind k) (default_of_kind k) expr
  ) handler extra_vars

and let_cont_rec env res conts body =
  let wrap, env = Env.flush_delayed_lets env in
  (* Compute the environment for jump ids *)
  let map = Continuation_handlers.to_map conts in
  let env = Continuation.Map.fold (fun k h acc ->
    snd (Env.add_jump_cont acc (continuation_arg_tys h) k)
  ) map env in
  (* Translate each continuation handler *)
  let map, res = Continuation.Map.fold (fun k h (map, res) ->
    let vars, handler, res = continuation_handler env res h in
    Continuation.Map.add k (vars, handler) map, res
  ) map (Continuation.Map.empty, res) in
  (* Setup the cmm handlers for the static catch *)
  let handlers = Continuation.Map.fold (fun k (vars, handle) acc ->
    let id = Env.get_jump_id env k in
    C.handler id vars handle :: acc
  ) map [] in
  let body, res = expr env res body in
  wrap (C.ccatch ~rec_flag:true ~body ~handlers), res

and continuation_handler_split h =
  let h = Continuation_handler.params_and_handler h in
  Continuation_params_and_handler.pattern_match h ~f:(fun args ~handler ->
    args, handler
  )

and continuation_arg_tys h =
  let args, _ = continuation_handler_split h in
  List.map machtype_of_kinded_parameter args

and continuation_handler env res h =
  let args, handler = continuation_handler_split h in
  let env, vars = var_list env args in
  let e, res = expr env res handler in
  vars, e, res

(* Function calls: besides the function calls, there are a few things to do:
   - setup the mutable variables for the exn cont extra args if needed
   - Flush the delayed let-bindings (this is not stricly necessary)
   - translate the call continuation (either through a jump, or inlining). *)
and apply_expr env res e =
  let call, env, effs = apply_call env e in
  let k_exn = Apply_expr.exn_continuation e in
  let call, env = wrap_call_exn env e call k_exn in
  let wrap, env = Env.flush_delayed_lets env in
  let body, res = wrap_cont env res effs call e in
  wrap body, res

(* Bare function calls *)
and apply_call env e =
  let f = Apply_expr.callee e in
  let dbg = Apply_expr.dbg e in
  let effs = Ece.all in
  match Apply_expr.call_kind e with
  (* Effects from arguments are ignored since a function call will always be
     given arbitrary effects and coeffects. *)
  | Call_kind.Function
      Call_kind.Function_call.Direct { code_id; closure_id = _; return_arity; } ->
    let env =
      Env.check_scope ~allow_deleted:false env
        (Code_id_or_symbol.Code_id code_id)
    in
    let info = Env.get_function_info env code_id in
    let ty = machtype_of_return_arity return_arity in
    let args, env, _ = arg_list env (Apply_expr.args e) in
    let args, env =
      match (info : Env.function_info option) with
      | None | Some { needs_closure_arg = true; } ->
        let f, env, _ = simple env f in
        args @ [f], env
      | Some { needs_closure_arg = false; } ->
        args, env
    in
    let f_code = symbol (Code_id.code_symbol code_id) in
    C.direct_call ~dbg ty (C.symbol f_code) args, env, effs
  | Call_kind.Function
      Call_kind.Function_call.Indirect_unknown_arity ->
    let f, env, _ = simple env f in
    let args, env, _ = arg_list env (Apply_expr.args e) in
    C.indirect_call ~dbg typ_val f args, env, effs
  | Call_kind.Function
      Call_kind.Function_call.Indirect_known_arity { return_arity; _ } ->
    let f, env, _ = simple env f in
    let args, env, _ = arg_list env (Apply_expr.args e) in
    let ty = machtype_of_return_arity return_arity in
    C.indirect_call ~dbg ty f args, env, effs
  | Call_kind.C_call { alloc; return_arity; _ } ->
    let f = function_name f in
    (* CR vlaviron: temporary hack to recover the right symbol *)
    let len = String.length f in
    assert (len >= 9);
    assert (String.sub f 0 9 = ".extern__");
    let f = String.sub f 9 (len - 9) in
    let args, env, _ = arg_list env (Apply_expr.args e) in
    let ty = machtype_of_return_arity return_arity in
    let wrap = wrap_extcall_result return_arity in
    wrap dbg (C.extcall ~dbg ~alloc f ty args), env, effs
  | Call_kind.Method { kind; obj; } ->
    let obj, env, _ = simple env obj in
    let meth, env, _ = simple env f in
    let kind = meth_kind kind in
    let args, env, _ = arg_list env (Apply_expr.args e) in
    C.send kind meth obj args dbg, env, effs

(* function calls that have an exn continuation with extra arguments
   must be wrapped with assignments for the mutable variables used
   to pass the extra arguments. *)
and wrap_call_exn env e call k_exn =
  let h_exn = Exn_continuation.exn_handler k_exn in
  let mut_vars = Env.get_exn_extra_args env h_exn in
  let extra_args = Exn_continuation.extra_args k_exn in
  if List.compare_lengths extra_args mut_vars = 0 then begin
    let aux (call, env) (arg, _k) v =
      let arg, env, _ = simple env arg in
      C.sequence (C.assign v arg) call, env
    in
    List.fold_left2 aux (call, env) extra_args mut_vars
  end else
    Misc.fatal_errorf "Length of [extra_args] in exception continuation %a@ \
                       does not match those in the environment (%a)@ for application \
                       expression:@ %a"
      Exn_continuation.print k_exn
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Ident.print)
      mut_vars
      Apply_expr.print e

(* Wrap a function call to honour its continuation *)
and wrap_cont env res effs call e =
  let k = Apply_expr.continuation e in
  if Continuation.equal (Env.return_cont env) k then
    call, res
  else begin
    match Env.get_k env k with
    | Jump { types = []; cont; } ->
      let wrap, _ = Env.flush_delayed_lets env in
      wrap (C.sequence call (C.cexit cont [] [])), res
    | Jump { types = [_]; cont; } ->
      let wrap, _ = Env.flush_delayed_lets env in
      wrap (C.cexit cont [call] []), res
    | Inline { handler_params = []; handler_body = body; _ } ->
      let var = Variable.create "*apply_res*" in
      let env = let_expr_bind body env var call effs in
      expr env res body
    | Inline { handler_params = [v]; handler_body = body; _ } ->
      let var = Kinded_parameter.var v in
      let env = let_expr_bind body env var call effs in
      expr env res body
    | Jump _
    | Inline _ ->
      (* TODO: add support using unboxed tuples *)
      Misc.fatal_errorf
        "Continuation %a should not handle multiple return values in@\n%a@\n%s"
        Continuation.print k Apply_expr.print e
        "Multi-arguments continuation across function calls are not yet supported"
  end

and apply_cont env res e =
  let k = Apply_cont_expr.continuation e in
  let args = Apply_cont_expr.args e in
  if Continuation.is_exn k then
    apply_cont_exn env res e k args
  else if Continuation.equal (Env.return_cont env) k then
    apply_cont_ret env res e k args
  else
    apply_cont_regular env res e k args

(* Exception Continuations always raise their first argument (which is
   supposed to be an exception). Additionally, they may have extra arguments
   that are passed to the handler via mutables variables (which are expected to be
   spilled on the stack). *)
and apply_cont_exn env res e k = function
  | exn :: extra ->
    let raise_kind =
      match Apply_cont_expr.trap_action e with
      | Some Pop {raise_kind; _} -> C.raise_kind raise_kind
      | _ ->
        Misc.fatal_errorf
          "Apply cont %a calls an exception cont without a Pop trap action"
          Apply_cont.print e
    in
    let exn, env, _ = simple env exn in
    let extra, env, _ = arg_list env extra in
    let mut_vars = Env.get_exn_extra_args env k in
    let wrap, _ = Env.flush_delayed_lets env in
    let expr =
      C.raise_prim raise_kind exn
        (Apply_cont_expr.debuginfo e)
    in
    let expr =
      List.fold_left2
        (fun expr arg v -> C.sequence (C.assign v arg) expr)
        expr extra mut_vars
    in
    wrap expr, res
  | [] ->
    Misc.fatal_errorf "Exception continuation %a has no arguments in@\n%a"
      Continuation.print k Apply_cont.print e

(* A call to the return continuation of the current block simply is the return value
   for the current block being translated. *)
and apply_cont_ret env res e k = function
  | [ret] ->
    let ret, env, _ = simple env ret in
    let wrap, _ = Env.flush_delayed_lets env in
    begin match Apply_cont_expr.trap_action e with
    | None ->
      wrap ret, res
    | Some (Pop _) ->
      wrap (C.trap_return ret [Cmm.Pop]), res
    | Some (Push _) ->
      Misc.fatal_errorf
        "Continuation %a (return cont) should not be applied with a push trap action"
        Continuation.print k
    end
  | _ ->
    (* TODO: add support using unboxed tuples *)
    Misc.fatal_errorf
      "Continuation %a (return cont) should be applied to a single argument in@\n%a@\n%s"
      Continuation.print k Apply_cont_expr.print e
      "Multi-arguments continuation across function calls are not yet supported"

and apply_cont_regular env res e k args =
  match Env.get_k env k with
  | Inline { handler_params; handler_body; } ->
    if not (Apply_cont_expr.trap_action e = None) then begin
      Misc.fatal_errorf "This [Apply_cont] should not have a trap \
                         action:@ %a"
        Apply_cont_expr.print e
    end;
    apply_cont_inline env res e k args handler_body handler_params
  | Jump { types; cont; } ->
    apply_cont_jump env res e types cont args

(* Inlining a continuation call simply needs to bind the arguments to the
   variables that the continuation's body expects. The delayed lets in the
   environment enables that translation to be tail-rec. *)
and apply_cont_inline env res e k args handler_body handler_params =
  if List.compare_lengths args handler_params = 0 then begin
    let vars = List.map Kinded_parameter.var handler_params in
    let args = List.map Named.create_simple args in
    let env, res =
      List.fold_left2 (let_expr_env handler_body) (env, res) vars args
    in
    expr env res handler_body
  end else
    Misc.fatal_errorf
      "Continuation %a in@\n%a@\nExpected %d arguments but got %a."
      Continuation.print k Apply_cont_expr.print e
      (List.length handler_params) Apply_cont_expr.print e;

  (* Continuation calls need to also translate the associated trap actions. *)
and apply_cont_jump env res e types cont args =
  if List.compare_lengths types args = 0 then begin
    let trap_actions = apply_cont_trap_actions env e in
    let args, env, _ = arg_list env args in
    let wrap, _ = Env.flush_delayed_lets env in
    wrap (C.cexit cont args trap_actions), res
  end else
    Misc.fatal_errorf "Types (%a) do not match arguments of@ %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
         Printcmm.machtype) types
      Apply_cont_expr.print e

and apply_cont_trap_actions env e =
  match Apply_cont_expr.trap_action e with
  | None -> []
  | Some (Pop _) -> [Cmm.Pop]
  | Some (Push { exn_handler; }) ->
    let cont = Env.get_jump_id env exn_handler in
    [Cmm.Push cont]

and switch env res s =
  let scrutinee = Switch.scrutinee s in
  let e, env, _ = simple env scrutinee in
  let arms = Switch.arms s in
  (* For binary switches, which can be translated to an if-then-else,
     it can be interesting to *not* untag the scrutinee (particularly
     for those coming from a source if-then-else on booleans) as that way
     the translation can use 2 instructions instead of 3.
     However, this is only useful to do if the tagged expression is
     smaller then the untagged one (which is not always true due to
     arithmetic simplifications performed by cmm_helpers).
     Additionally for switches with more than 2 arms, not untagging
     and lifting the switch to perform on tagged integer might be worse
     (because the discriminant of the arms may not be successive anymore,
     thus preventing the use of a table), or simply not worth it
     given the already high number of instructions needed for big
     switches (but that might be up-to-debate on small switches with
     3-5 arms). *)
  let scrutinee, tag_discriminant =
    match Target_imm.Map.cardinal arms with
    | 2 ->
      begin match match_var_with_extra_info env scrutinee with
      | None -> e, false
      | Some Untag e' ->
        let size_e = cmm_arith_size e in
        let size_e' = cmm_arith_size e' in
        if size_e' < size_e then e', true else e, false
      end
    | _ -> e, false
  in
  make_switch ~tag_discriminant env res scrutinee arms

and match_var_with_extra_info env simple : Env.extra_info option =
  Simple.pattern_match simple
    ~const:(fun _ -> None)
    ~name:(fun n ->
      Name.pattern_match n
        ~symbol:(fun _ -> None)
        ~var:(Env.extra_info env)
    )

(* Small function to estimate the number of arithmetic instructions
   in a cmm expression. This is currently used to determine whether
   untagging an expression resulted in a smaller expression or not
   (as can happen because of some arithmetic simplifications performed
   by cmm_helpers.ml) *)
and cmm_arith_size e =
  match (e : Cmm.expression) with
  | Cop (
    ( Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi
    | Cand | Cor | Cxor | Clsl | Clsr | Casr ), l, _) ->
    List.fold_left (+) 1 @@
    List.map cmm_arith_size l
  | _ -> 0

and prepare_discriminant ~tag d =
  let targetint_d = Target_imm.to_targetint' d in
  let prepared_d =
    if tag then tag_targetint targetint_d else targetint_d
  in
  int_of_targetint prepared_d

and make_arm ~tag_discriminant env res (d, action) =
  let d = prepare_discriminant ~tag:tag_discriminant d in
  let cmm_action, res = apply_cont env res action in
  (d, cmm_action), res

(* Create a switch given the env, the scrutinee and its arms,
   plus some optimization/simplification option, for now:
   - whether to tag the discriminant of the arms (this suppose
     that the scrutinee is adequately tagged/untagged) *)
and make_switch ~tag_discriminant env res e arms =
  let wrap, env = Env.flush_delayed_lets env in
  match Target_imm.Map.cardinal arms with

  (* Binary case: if-then-else *)
  | 2 ->
    let aux = make_arm ~tag_discriminant env in
    let first_arm, res = aux res @@ Target_imm.Map.min_binding arms in
    let second_arm, res = aux res @@ Target_imm.Map.max_binding arms in
    begin match first_arm, second_arm with
    (* These switchs are actually if-then-elses.
       On such switches, transl_switch_clambda will introduce a let-binding
       to the scrutinee before creating an if-then-else, introducing an
       indirection that might prevent some optimizations performed by
       selectgen/emit when the condition is inlined in the if-then-else. *)
    | (0, else_), (_, then_)
    | (_, then_), (0, else_)
      -> wrap (C.ite e ~then_ ~else_), res
    (* Similar case to the if/then/else but none of the arms match 0,
       so we have to generate an equality test, and make sure it is inside
       the condition to ensure selectgen and emit can take advantage of it. *)
    | (x, if_x), (_, if_not)
      -> wrap (C.ite (C.eq (C.int x) e) ~then_:if_x ~else_:if_not), res
    end

  (* General case *)
  | n ->
    (* The transl_switch_clambda expects an index array such that
       index.(d) is the index in [cases] of the expression to
       execute when [e] matches [d]. *)
    let (max_d, _) = Target_imm.Map.max_binding arms in
    let m = prepare_discriminant ~tag:tag_discriminant max_d in
    let cases = Array.make (n + 1) C.unreachable in
    let index = Array.make (m + 2) n in
    let _, res =
      Target_imm.Map.fold (fun discriminant action (i, res) ->
        let (d, cmm_action), res =
          make_arm ~tag_discriminant env res (discriminant, action)
        in
        cases.(i) <- cmm_action;
        index.(d) <- i;
        i + 1, res
      ) arms (0, res)
    in
    wrap (C.transl_switch_clambda Location.none e index cases), res

and invalid _env res _e =
  C.unreachable, res

and set_of_closures env res s =
  let fun_decls = Set_of_closures.function_decls s in
  let decls = Function_declarations.funs fun_decls in
  let elts = filter_closure_vars env (Set_of_closures.closure_elements s) in
  let layout = Env.layout env
                 (List.map fst (Closure_id.Map.bindings decls))
                 (List.map fst (Var_within_closure.Map.bindings elts))
  in
  let l, env, effs = fill_layout decls elts env Ece.pure [] 0 layout in
  { cmm = C.make_closure_block l; env; effs; res; }

and fill_layout decls elts env effs acc i = function
  | [] -> List.rev acc, env, effs
  | (j, slot) :: r ->
    let acc = fill_up_to j acc i in
    let acc, offset, env, eff = fill_slot decls elts env acc j slot in
    let effs = Ece.join eff effs in
    fill_layout decls elts env effs acc offset r

and fill_slot decls elts env acc offset slot =
  match (slot : Un_cps_closure.layout_slot) with
  | Infix_header ->
    let field = C.alloc_infix_header (offset + 1) Debuginfo.none in
    field :: acc, offset + 1, env, Ece.pure
  | Env_var v ->
    let field, env, eff = simple env (Var_within_closure.Map.find v elts) in
    field :: acc, offset + 1, env, eff
  | Closure (c : Closure_id.t) ->
    let c : Closure_id.t = c in
    let decl = Closure_id.Map.find c decls in
    let dbg = Function_declaration.dbg decl in
    let arity = List.length (Function_declaration.params_arity decl) in
    let code_id = Function_declaration.code_id decl in
    let code_symbol = Code_id.code_symbol code_id in
    let code_name = Linkage_name.to_string (Symbol.linkage_name code_symbol) in
    (* We build here the **reverse** list of fields for the closure *)
    if arity = 1 || arity = 0 then begin
      let acc =
        C.int_const dbg arity ::
        C.symbol ~dbg code_name ::
        acc
      in
      acc, offset + 2, env, Ece.pure
    end else begin
      let acc =
        C.symbol ~dbg code_name ::
        C.int_const dbg arity ::
        C.symbol ~dbg (C.curry_function_sym arity) ::
        acc
      in
      acc, offset + 3, env, Ece.pure
    end

and fill_up_to j acc i =
  assert (i <= j);
  if i = j then acc
  else fill_up_to j (C.int 1 :: acc) (i + 1)

(* Translate a function declaration. *)

and params_and_body env res fun_name p =
  Function_params_and_body.pattern_match p
    ~f:(fun ~return_continuation:k k_exn vars ~body ~my_closure ->
      try
        let args = function_args vars my_closure body in
        let k_exn = Exn_continuation.exn_handler k_exn in
        (* Init the env and create a jump id for the ret closure
           in case a trap action is attached to one of tis call *)
        let env = Env.enter_function_def env k k_exn in
        (* translate the arg list and body *)
        let env, fun_args = var_list env args in
        let fun_body, res = expr env res body in
        let fun_flags = function_flags () in
        let fun_dbg = Function_params_and_body.debuginfo p in
        C.fundecl fun_name fun_args fun_body fun_flags fun_dbg, res
      with Misc.Fatal_error as e ->
        Format.eprintf "\n%sContext is:%s translating function %s to Cmm \
                        with return cont %a, exn cont %a and body:@ %a\n"
          (Flambda_colours.error ())
          (Flambda_colours.normal ())
          fun_name
          Continuation.print k
          Exn_continuation.print k_exn
          Expr.print body;
        raise e)

(* CR gbury: for the future, try and rearrange the generated cmm
   code to move assignments closer to the variable definitions
   Or better: add traps to the env to insert assignemnts after
   the variable definitions. *)

(* Note about the root symbol: it does not need any particular treatment.
   Concerning gc_roots, it's like any other statically allocated symbol: if it
   has an associated computation, then it will already be included in the list
   of gc_roots; otherwise it does not *have* to be a root. *)

(* Compilation units *)

let unit (middle_end_result : Flambda2_middle_end.middle_end_result) =
  let unit = middle_end_result.unit in
  let offsets =
    match middle_end_result.cmx with
    | None -> Exported_offsets.imported_offsets ()
    | Some cmx -> Flambda_cmx_format.exported_offsets cmx
  in
  Profile.record_call "flambda2_to_cmm" (fun () ->
    let offsets = Un_cps_closure.compute_offsets offsets unit in
    begin match middle_end_result.cmx with
    | None -> () (* Either opaque was passed, or there is no need to export
                    offsets *)
    | Some cmx ->
      let cmx = Flambda_cmx_format.with_exported_offsets cmx offsets in
      Compilenv.(set_global_info (Flambda2 cmx))
    end;
    let used_closure_vars = Flambda_unit.used_closure_vars unit in
    let dummy_k = Continuation.create () in
    (* The dummy continuation is passed here since we're going to manually
       arrange that the return continuation turns into "return unit".
       (Module initialisers return the unit value). *)
    let env =
      Env.mk offsets dummy_k
        (Flambda_unit.exn_continuation unit)
        used_closure_vars
    in
    let _env, return_cont_params =
      (* Note: the environment would be used if we needed to compile the
         handler, but since it's constant we don't need it *)
      var_list env [
        Kinded_parameter.create (Variable.create "*ret*")
          Flambda_kind.value;
      ]
    in
    let return_cont, env =
      Env.add_jump_cont env (List.map snd return_cont_params)
        (Flambda_unit.return_continuation unit)
    in
    let body, res = expr env R.empty (Flambda_unit.body unit) in
    let body =
      let unit_value = C.targetint Targetint.one in
      C.ccatch
        ~rec_flag:false ~body
        ~handlers:[C.handler return_cont return_cont_params unit_value]
    in
    let entry =
      let dbg = Debuginfo.none in
      let fun_name = Compilenv.make_symbol (Some "entry") in
      let fun_codegen =
        if Flambda_features.backend_cse_at_toplevel () then
          [ Cmm.Reduce_code_size; ]
        else
          [ Cmm.Reduce_code_size; Cmm.No_CSE; ]
      in
      C.cfunction (C.fundecl fun_name [] body fun_codegen dbg)
    in
    let data, gc_roots, functions = R.to_cmm res in
    let cmm_data = C.flush_cmmgen_state () in
    let roots = List.map symbol gc_roots in
    (C.gc_root_table roots) :: data @ cmm_data @ functions @ [entry]
    (* Misc.fatal_error "To be continued" *)
    (* let functions = program_functions offsets used_closure_vars unit in *)
  )
