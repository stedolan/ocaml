(* TEST
   * expect
*)

type ('a : immediate) imm = 'a

(* FIXME_layout: should this refine the value layout to immediate here?
   (See a similar FIXME_layout in decl.ml) *)
module type T = sig
  val x : ('a : value) . 'a -> 'a imm
end
[%%expect{|
type ('a : immediate) imm = 'a
module type T = sig val x : ('a : immediate). 'a -> 'a imm end
|}]

module type T = sig
  val x : ('a : float) . 'a -> 'a imm
end
[%%expect{|
Line 2, characters 31-33:
2 |   val x : ('a : float) . 'a -> 'a imm
                                   ^^
Error: This type 'a should be an instance of type 'b
       A type with layout immediate was expected,
       but one with layout float was provided
|}]

module M : sig
  type t = { id : ('a : immediate) . 'a -> 'a }
end = struct
  type t = { id : 'a . 'a -> 'a }
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { id : 'a . 'a -> 'a }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { id : 'a. 'a -> 'a; } end
       is not included in
         sig type t = { id : ('a : immediate). 'a -> 'a; } end
       Type declarations do not match:
         type t = { id : 'a. 'a -> 'a; }
       is not included in
         type t = { id : ('a : immediate). 'a -> 'a; }
       Fields do not match:
         id : 'a. 'a -> 'a;
       is not compatible with:
         id : ('a : immediate). 'a -> 'a;
       The types are not equal.
|}]


module M : sig
  type t : immediate
end = struct
  type t = string
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = string
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = string end
       is not included in
         sig type t : immediate end
       Type declarations do not match:
         type t = string
       is not included in
         type t : immediate
       Their layouts are incompatible.
|}]


(* FIXME_layout: Bad error message here. ("Constraints"?) *)
module M : sig
  type 'a t
end = struct
  type ('a : immediate) t = int
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : immediate) t = int
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a : immediate) t = int end
       is not included in
         sig type 'a t end
       Type declarations do not match:
         type ('a : immediate) t = int
       is not included in
         type 'a t
       Their constraints differ.
|}]

module M : sig
  type ('a : value) t : immediate
end = struct
  type ('a : immediate) t = 'a
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : immediate) t = 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a : immediate) t = 'a end
       is not included in
         sig type 'a t : immediate end
       Type declarations do not match:
         type ('a : immediate) t = 'a
       is not included in
         type 'a t : immediate
       Their constraints differ.
|}]

(* FIXME_layout: I think it would be sound to allow this,
   restricting the type-level function t to a subset of its
   domain. Is this something we want to do? *)
module M : sig
  type ('a : immediate) t : immediate
end = struct
  type ('a : value) t = 'a
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : value) t = 'a
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a end
       is not included in
         sig type ('a : immediate) t : immediate end
       Type declarations do not match:
         type 'a t = 'a
       is not included in
         type ('a : immediate) t : immediate
       Their layouts are incompatible.
|}]




(* A type scheme is more general if its
   variables quantify over larger layouts
*)

module M : sig
  val f : 'a -> 'a
  val g : ('a : immediate) . 'a -> 'a
end = struct
  let f : ('a : value) . 'a -> 'a = fun x -> x
  let g : ('a : value) . 'a -> 'a = fun x -> x
end
[%%expect{|
module M : sig val f : 'a -> 'a val g : ('a : immediate). 'a -> 'a end
|}]

module M : sig
  val f : ('a : value) . 'a -> 'a
end = struct
  let f : ('a : immediate) . 'a -> 'a = fun x -> x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f : ('a : immediate) . 'a -> 'a = fun x -> x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : ('a : immediate). 'a -> 'a end
       is not included in
         sig val f : 'a -> 'a end
       Values do not match:
         val f : ('a : immediate). 'a -> 'a
       is not included in
         val f : 'a -> 'a
|}]


(* Layout checks are required for well-formed signatures *)

module type A = sig type foo : immediate = string end
[%%expect{|
Line 1, characters 31-40:
1 | module type A = sig type foo : immediate = string end
                                   ^^^^^^^^^
Error: This type does not have layout immediate
|}]
module type A = sig type foo = string imm end
[%%expect{|
Line 1, characters 31-37:
1 | module type A = sig type foo = string imm end
                                   ^^^^^^
Error: This type string should be an instance of type 'a
       A type with layout immediate was expected,
       but one with layout value was provided
|}]


(* FIXME_layout: error message isn't great here (location is dubious) *)
module type A = sig
  type s = t and t = [`Foo of s imm]
end
[%%expect{|
Line 2, characters 2-12:
2 |   type s = t and t = [`Foo of s imm]
      ^^^^^^^^^^
Error: This type constructor expands to type s but is used here with type 'a
       A type with layout immediate was expected,
       but one with layout value was provided
|}]


module type A = sig
  type s = r and t = [`Foo of s imm] and r = int
end
[%%expect{|
module type A = sig type s = r and t = [ `Foo of s imm ] and r = int end
|}]


type ('a : immediate) k = int
module type A = sig
  type s = r k and r = s imm
end

[%%expect{|
type ('a : immediate) k = int
module type A = sig type s = r k and r = s imm end
|}]


(* with constraints *)

module type T = sig type t : immediate end
module type M = T with type t = int
[%%expect{|
module type T = sig type t : immediate end
module type M = sig type t = int end
|}]
module type N = T with type t = string
[%%expect{|
Line 1, characters 16-38:
1 | module type N = T with type t = string
                    ^^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = string
       is not included in
         type t : immediate
       Their layouts are incompatible.
|}]
module type N = T with type t := string
[%%expect{|
Line 1, characters 16-39:
1 | module type N = T with type t := string
                    ^^^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = string
       is not included in
         type t : immediate
       Their layouts are incompatible.
|}]
