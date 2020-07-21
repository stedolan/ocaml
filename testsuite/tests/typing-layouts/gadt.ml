(* TEST
   * expect
*)

type ('a : immediate) imm = 'a

(* FIXME_layout: There should be a better syntax to constrain the
   layout of 'b *)
type ('a : value) is_immediate =
  | Immediate : ('b : immediate) . 'b is_immediate
  | Value : 'a is_immediate

[%%expect{|
type ('a : immediate) imm = 'a
type 'a is_immediate =
    Immediate : 'b imm is_immediate
  | Value : 'a is_immediate
|}]

module Ref : sig
  val mkimm : ('a : immediate) . 'a -> 'a ref
end = struct
  let mkimm = ref
end

let use_imm (type a) (e : a is_immediate) (x : a) : a ref =
  match e with
  | Immediate -> Ref.mkimm x
  | Value -> ref x

[%%expect{|
module Ref : sig val mkimm : ('a : immediate). 'a -> 'a ref end
val use_imm : 'a is_immediate -> 'a -> 'a ref = <fun>
|}]
