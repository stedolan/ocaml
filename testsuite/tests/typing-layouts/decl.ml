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

type bad_t : asdjfioasj = string
[%%expect{|
Line 1, characters 13-23:
1 | type bad_t : asdjfioasj = string
                 ^^^^^^^^^^
Error: Unknown layout asdjfioasj
|}]

type 'a t : immediate = T of 'a
[%%expect{|
Line 1, characters 12-21:
1 | type 'a t : immediate = T of 'a
                ^^^^^^^^^
Error: This type does not have layout immediate
|}]

(* FIXME_layout: this should be allowed *)
type 'a t : immediate = Foo | Bar
[%%expect{|
Line 1, characters 12-21:
1 | type 'a t : immediate = Foo | Bar
                ^^^^^^^^^
Error: This type does not have layout immediate
|}]



(* FIXME_layout: is this the right behaviour?
   Should type parameters be like unification variables (whose layout might refine)
   or like univars (whose layout stays rigid)?
   Should there be a distinction between types where an explicit layout is
   specified and those where none is? *)
type ('a : value) t = { foo : 'a t_imm }

[%%expect{|
type ('a : immediate) t = { foo : 'a t_imm; }
|}]


(* Regardless of the above, this should definitely not be allowed *)
type ('a : float) bad_t = { foo : 'a t_imm }

[%%expect{|
Line 1, characters 34-36:
1 | type ('a : float) bad_t = { foo : 'a t_imm }
                                      ^^
Error: This type 'a should be an instance of type 'a0
       A type with layout immediate was expected,
       but one with layout float was provided
|}]

