
(* open Std_internal *)

module Type_equal = struct
  type (_,_) t = Refl : ('a,'a) t
end

    module type Named_T0 = sig
      type named
      type t
      val typename_of_named : named
      val typename_of_t : t
      val witness : (t, named) Type_equal.t
    end

module Type_generic_intf = struct
  module M (X : sig type 'a t end) = struct
  module type S = sig
    type t
    val compute : t X.t
  end
  end

end

module Typename : sig
  type 'a t = 'a
  val same_witness_exn : 'a t -> 'b t -> ('a, 'b) Type_equal.t
end = struct
  type 'a t = 'a
  let same_witness_exn = assert false
end
module Typerep = struct
  type 'a t
end

module type Named = sig
  type 'a computation
  module Context : sig
    type t
    val create : unit -> t
  end
  type 'a t
  val init : Context.t -> 'a Typename.t -> 'a t
  val get_wip_computation : 'a t -> 'a computation
  val set_final_computation : 'a t -> 'a computation -> 'a computation
  val share : _ Typerep.t -> bool
end

module type Computation = sig
  type 'a t

  val int : int t
  val int32 : int32 t
  val int64 : int64 t
  val nativeint : nativeint t
  val char : char t
  val float : float t
  val string : string t
  val bytes : bytes t
  val bool : bool t
  val unit : unit t
  val option : 'a t -> 'a option t
  val list : 'a t -> 'a list t
  val array : 'a t -> 'a array t
  val lazy_t : 'a t -> 'a lazy_t t
  val ref_ : 'a t -> 'a ref t
  val function_ : 'a t -> 'b t -> ('a -> 'b) t
  val tuple2 : 'a t -> 'b t -> ('a * 'b) t
  val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val tuple5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t

  module Named : Named with type 'a computation := 'a t
end

module Ident = struct
  type t = {
    name : string;
  }
  exception Broken_dependency of string
end

(* Extending an existing generic *)
module type Extending = sig

  type 'a t
  type 'a computation = 'a t

  val ident : Ident.t

  (* generic_ident * typename or info *)
  exception Not_implemented of string * string

  module type S = sig
    type t
    val compute : t computation
  end

end

(* Implementing a new generic *)
module type S_implementation = sig

  include Extending

  (* raise using the current ident *)
  val raise_not_implemented : string -> 'a

  type implementation = {
    generic : 'a. 'a Typerep.t -> 'a computation;
  }
end

module type S = sig
  include Extending
  val of_typerep : 'a Typerep.t -> [ `generic of 'a computation ]
  module Computation : Computation with type 'a t = 'a t
end

module Make_S_implementation(X : sig
  type 'a t
  val name : string
  val required : Ident.t list
end) : S_implementation with type 'a t = 'a X.t = struct
  type 'a t = 'a X.t
  type 'a computation = 'a t

  include Type_generic_intf.M(struct type 'a t = 'a computation end)

  let ident = { Ident.
    name = X.name;
  }

    let compute () =
      match assert false with
      | None -> None
      | Some rep ->
        let module S = (val rep : S) in
        let witness = Typename.same_witness_exn S.typename_of_t T.typename_of_named in
        let module L = Type_equal.Lift(struct
          type 'a t = 'a computation
        end) in
        Some (Type_equal.conv (L.lift witness) S.compute)
end
