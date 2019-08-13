(* TEST
   * expect
*)

type ('a : immediate) imm = 'a


type ('a : value) is_immediate =
  | Immediate : 'a imm is_immediate
  | Value : 'a is_immediate

[%%expect{|
type ('a : immediate) imm = 'a
type 'a is_immediate =
    Immediate : 'a imm is_immediate
  | Value : 'a is_immediate
|}]

module Ref : sig
  val mkimm : ('a : immediate) . 'a -> 'a ref
end = struct
  let mkimm = ref
end

(* FIXME: It would be cool if this worked! *)

let use_imm (type a) (e : a is_immediate) (x : a) : a ref =
  match e with
  | Immediate -> Ref.mkimm a
  | Value -> ref a

[%%expect{|
module Ref : sig val mkimm : ('a : immediate). 'a -> 'a ref end
Line 11, characters 4-13:
11 |   | Immediate -> Ref.mkimm a
         ^^^^^^^^^
Error: This pattern matches values of type 'a imm is_immediate
       but a pattern was expected which matches values of type a is_immediate
       Type 'a imm = 'a is not compatible with type a
       A type with layout immediate was expected,
       but one with layout value was provided
|}]
