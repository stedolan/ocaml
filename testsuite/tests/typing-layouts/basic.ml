(* TEST
   * expect
*)

type 'a t = T of 'a
type ('a : immediate) t_imm = Timm of 'a
type 'a id = 'a
type ('a : immediate) id_imm = 'a

[%%expect{|
type 'a t = T of 'a
type ('a : immediate) t_imm = Timm of 'a
type 'a id = 'a
type ('a : immediate) id_imm = 'a
|}]

let f1 x = T x
let f2 x = Timm x
let f3 x : _ id = x
let f4 x : _ id_imm = x

[%%expect{|
val f1 : 'a -> 'a t = <fun>
val f2 : ('a : immediate). 'a -> 'a t_imm = <fun>
val f3 : 'a id -> 'a id = <fun>
val f4 : ('a : immediate). 'a id_imm -> 'a id_imm = <fun>
|}]

let f1 = function T x -> Timm x
let f2 = function Timm x -> T x

[%%expect{|
val f1 : ('a : immediate). 'a t -> 'a t_imm = <fun>
val f2 : ('a : immediate). 'a t_imm -> 'a t = <fun>
|}]


(* Type definitions.
   Layouts of parameters can be constrained by their definition,
   but layouts of univars cannot.
   (This matches type constraints, I think) *)

type ('a : immediate) t_imm_1 = 'a t_imm
type 'a t_imm_2 = 'a t_imm
type ('a : value) t_imm_3 = 'a t_imm
[%%expect{|
type ('a : immediate) t_imm_1 = 'a t_imm
type ('a : immediate) t_imm_2 = 'a t_imm
type ('a : immediate) t_imm_3 = 'a t_imm
|}]

type p_imm_1 = { foo_1 : ('a : immediate) . 'a t_imm }
[%%expect{|
type p_imm_1 = { foo_1 : ('a : immediate). 'a t_imm; }
|}]
type p_imm_2 = { foo_1 : 'a . 'a t_imm }
[%%expect{|
Line 1, characters 25-38:
1 | type p_imm_2 = { foo_1 : 'a . 'a t_imm }
                             ^^^^^^^^^^^^^
Error: The universal type variable 'a cannot be generalized:
       it has layout immediate, but should have value.
|}]
type p_imm_3 = { foo_1 : ('a : value) . 'a t_imm }
[%%expect{|
Line 1, characters 25-48:
1 | type p_imm_3 = { foo_1 : ('a : value) . 'a t_imm }
                             ^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a cannot be generalized:
       it has layout immediate, but should have value.
|}]

(* Newtypes *)

let f1 (type a) (x : a) : a t = T x

[%%expect{|
val f1 : 'a -> 'a t = <fun>
|}]


let f1 (type a : immediate) (x : a) : a t = T x

[%%expect{|
val f1 : ('a : immediate). 'a -> 'a t = <fun>
|}]


let f2 (type a) (x : a) : a t_imm = Timm x

[%%expect{|
Line 1, characters 26-27:
1 | let f2 (type a) (x : a) : a t_imm = Timm x
                              ^
Error: This type a should be an instance of type 'a
       A type with layout immediate was expected,
       but one with layout value was provided
|}]

let f2 (type a : immediate) (x : a) : a t_imm = Timm x

[%%expect{|
val f2 : ('a : immediate). 'a -> 'a t_imm = <fun>
|}]

let f3 (type a) (x : a) : a id = x

[%%expect{|
val f3 : 'a -> 'a id = <fun>
|}]

let f3 (type a : immediate) (x : a) : a id = x

[%%expect{|
val f3 : ('a : immediate). 'a -> 'a id = <fun>
|}]

let f4 (type a) (x : a) : a id_imm = x

[%%expect{|
Line 1, characters 26-27:
1 | let f4 (type a) (x : a) : a id_imm = x
                              ^
Error: This type a should be an instance of type 'a
       A type with layout immediate was expected,
       but one with layout value was provided
|}]

let f4 (type a : immediate) (x : a) : a id_imm = x

[%%expect{|
val f4 : ('a : immediate). 'a -> 'a id_imm = <fun>
|}]


(* Polymorphic recursion *)

let rec loop1 : 'a . 'a -> 'a id = fun x -> loop1 x
[%%expect{|
val loop1 : 'a -> 'a id = <fun>
|}]


let rec loop2 : 'a . 'a -> 'a id_imm = fun x -> loop2 x
[%%expect{|
Line 1, characters 16-36:
1 | let rec loop2 : 'a . 'a -> 'a id_imm = fun x -> loop2 x
                    ^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a cannot be generalized:
       it has layout immediate, but should have value.
|}]


let id_imm : ('a : immediate) . 'a -> 'a = fun x -> x
let rec loop3 : 'a . 'a -> 'a = fun x -> id_imm x
 (* FIXME_layout: this error message isn't great *)
[%%expect{|
val id_imm : ('a : immediate). 'a -> 'a = <fun>
Line 2, characters 32-49:
2 | let rec loop3 : 'a . 'a -> 'a = fun x -> id_imm x
                                    ^^^^^^^^^^^^^^^^^
Error: This definition has type 'b -> 'b which is less general than
         'a. 'a -> 'a
|}]

(* Same again, with object types *)

class c = object
  method loop1 : 'a . 'a -> 'a id = fun x -> loop1 x
end
[%%expect{|
class c : object method loop1 : 'a -> 'a id end
|}]

class c = object
  method loop2 : 'a . 'a -> 'a id_imm = fun x -> loop2 x
end
[%%expect{|
Line 2, characters 17-37:
2 |   method loop2 : 'a . 'a -> 'a id_imm = fun x -> loop2 x
                     ^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a cannot be generalized:
       it has layout immediate, but should have value.
|}]

class c = object
  method loop3 : 'a . 'a -> 'a = fun x -> id_imm x
end
[%%expect{|
Line 2, characters 33-50:
2 |   method loop3 : 'a . 'a -> 'a = fun x -> id_imm x
                                     ^^^^^^^^^^^^^^^^^
Error: This method has type 'b -> 'b which is less general than 'a. 'a -> 'a
|}]
