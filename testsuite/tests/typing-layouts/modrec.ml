
module Type_equal = struct
  type (_,_) t = Refl : ('a,'a) t
end

module Variant_and_record_intf = struct

module M (X : sig
  type 'a t
end) = struct

  module Tag_internal = struct
    type ('variant, 'args) create = Args of ('args -> 'variant) | Const of 'variant
    type ('variant, 'args) t =
      { label : string
      ; rep : 'args X.t
      ; arity : int
      ; args_labels: string list
      ; index : int
      ; ocaml_repr : int
      ; create : ('variant, 'args) create
      }
  end

  module Tag : sig
    type ('variant, 'args) create = Args of ('args -> 'variant) | Const of 'variant
    type ('variant, 'args) t
    val label : (_, _) t -> string

    val arity : (_, _) t -> int
    val args_labels : (_, _) t -> string list
    val index : (_, _) t -> int
    val ocaml_repr : (_, _) t -> int
    val create : ('variant, 'args) t -> ('variant, 'args) create
    val traverse : (_, 'args) t -> 'args X.t
    val internal_use_only : ('a, 'b) Tag_internal.t -> ('a, 'b) t
  end = struct
    include Tag_internal
    let label t = t.label
    let arity t = t.arity
    let args_labels t = t.args_labels
    let index t = t.index
    let ocaml_repr t = t.ocaml_repr
    let create t = t.create
    let traverse t = t.rep
    let internal_use_only t = t
  end

  module Variant_internal = struct
    type _ tag = Tag : ('variant, 'a) Tag.t -> 'variant tag
    type _ value = Value : ('variant, 'a) Tag.t * 'a -> 'variant value
    type 'a t = {
      tags : 'a tag array;
      polymorphic : bool;
      value : 'a -> 'a value;
    }
  end

  module Variant : sig
    type _ tag = Tag : ('variant, 'args) Tag.t -> 'variant tag
    type _ value = Value : ('variant, 'args) Tag.t * 'args -> 'variant value
    type 'a t
    val length : 'a t -> int
    val tag : 'a t -> int -> 'a tag
    val is_polymorphic : _ t -> bool
    val value : 'a t -> 'a -> 'a value
    val fold : 'a t -> init:'acc -> f:('acc -> 'a tag -> 'acc) -> 'acc
    val internal_use_only : 'a Variant_internal.t -> 'a t
  end = struct
    include Variant_internal
    let length t = Array.length t.tags
    let tag t index = t.tags.(index)
    let is_polymorphic t = t.polymorphic
    let value t = t.value

    let fold t ~init ~f = Array.fold_left f init t.tags

    let internal_use_only t = t
  end

  module Field_internal = struct
    type ('record, 'field) t = {
      label : string;
      rep : 'field X.t;
      index : int;
      get : ('record -> 'field);
      (* set : ('record -> 'field -> unit) option; (\* mutable field *\) *)
      is_mutable : bool;
    }
  end

  module Field : sig
    type ('record, 'field) t
    val label : (_, _) t -> string
    val index : (_, _) t -> int
    val get : ('record, 'field) t -> 'record -> 'field
    val is_mutable : (_, _) t -> bool
    val traverse : (_, 'field) t -> 'field X.t
    val internal_use_only : ('a, 'b) Field_internal.t -> ('a, 'b) t
  end = struct
    include Field_internal
    let label t = t.label
    let index t = t.index
    let get t = t.get
    let is_mutable t = t.is_mutable
    let traverse t = t.rep

    let internal_use_only t = t
  end

  module Record_internal = struct
    type _ field = Field : ('record, 'a) Field.t -> 'record field
    type 'record fields = { get : 'field. ('record, 'field) Field.t -> 'field }
    type 'a t = {
      fields : 'a field array;
      has_double_array_tag : bool;
      create : 'a fields -> 'a;
    }
  end

  module Record : sig
    type _ field = Field : ('record, 'a) Field.t -> 'record field
    type 'record fields = { get : 'field. ('record, 'field) Field.t -> 'field }
    type 'a t
    val length : 'a t -> int
    val field : 'a t -> int -> 'a field
    val has_double_array_tag : _ t -> bool
    val create : 'a t -> 'a fields -> 'a
    val fold : 'a t -> init:'acc -> f:('acc -> 'a field -> 'acc) -> 'acc
    val internal_use_only : 'a Record_internal.t -> 'a t
  end = struct
    include Record_internal
    let length t = Array.length t.fields
    let field t index = t.fields.(index)
    let has_double_array_tag t = t.has_double_array_tag
    let create t = t.create
    let fold t ~init ~f = Array.fold_left f init t.fields

    let internal_use_only t = t
  end
end

module type S = sig
  type 'a t
  include (module type of M (struct type 'a rep = 'a t type 'a t = 'a rep end))
end
end

module rec Typerep : sig

  type _ t =
    | Int        : int t
    | Int32      : int32 t
    | Int64      : int64 t
    | Nativeint  : nativeint t
    | Char       : char t
    | Float      : float t
    | String     : string t
    | Bytes      : bytes t
    | Bool       : bool t
    | Unit       : unit t
    | Option     : 'a t -> 'a option t
    | List       : 'a t -> 'a list t
    | Array      : 'a t -> 'a array t
    | Lazy       : 'a t -> 'a lazy_t t
    | Ref        : 'a t -> 'a ref t
    | Function   : ('dom t * 'rng t) -> ('dom -> 'rng) t
    | Tuple      : 'a Typerep.Tuple.t -> 'a t
    | Record     : 'a Typerep.Record.t -> 'a t
    | Variant    : 'a Typerep.Variant.t -> 'a t
    | Named      : ('a Typerep.Named.t * 'a t lazy_t option) -> 'a t

  type packed = T : 'a t -> packed

  module Named : sig
    module type T0 = sig
      type named
      type t
      val witness : (t, named) Type_equal.t
    end
    module type T1 = sig
      type 'a named
      type a val a : a Typerep.t
      type t
      val witness : (t, a named) Type_equal.t
    end
    module type T2 = sig
      type ('a, 'b) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type t
      val witness : (t, (a, b) named) Type_equal.t
    end
    module type T3 = sig
      type ('a, 'b, 'c) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type c val c : c Typerep.t
      type t
      val witness : (t, (a, b, c) named) Type_equal.t
    end
    module type T4 = sig
      type ('a, 'b, 'c, 'd) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type c val c : c Typerep.t
      type d val d : d Typerep.t
      type t
      val witness : (t, (a, b, c, d) named) Type_equal.t
    end
    module type T5 = sig
      type ('a, 'b, 'c, 'd, 'e) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type c val c : c Typerep.t
      type d val d : d Typerep.t
      type e val e : e Typerep.t
      type t
      val witness : (t, (a, b, c, d, e) named) Type_equal.t
    end
    (* there the module is necessary because we need to deal with a type [t] with
       parameters whose kind is not representable as a type variable: ['a 't], even with
       a gadt. *)
    type 'a t =
    | T0 of (module T0 with type t = 'a)
    | T1 of (module T1 with type t = 'a)
    | T2 of (module T2 with type t = 'a)
    | T3 of (module T3 with type t = 'a)
    | T4 of (module T4 with type t = 'a)
    | T5 of (module T5 with type t = 'a)

    val arity : _ t -> int
  end

  module Tuple : sig
    (* these constructors could be plunged at toplevel of Typerep.t, however it is less
       verbose that way *)
    type _ t =
    | T2 : ('a Typerep.t * 'b Typerep.t)
      -> ('a * 'b) t
    | T3 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t)
      -> ('a * 'b * 'c) t
    | T4 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t)
      -> ('a * 'b * 'c * 'd) t
    | T5 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t * 'e Typerep.t)
      -> ('a * 'b * 'c * 'd * 'e) t

    val arity : _ t -> int
  end

  include Variant_and_record_intf.S with type 'a t := 'a Typerep.t

end = struct

  type _ t =
    | Int : int t
    | Int32 : int32 t
    | Int64 : int64 t
    | Nativeint : nativeint t
    | Char : char t
    | Float : float t
    | String : string t
    | Bytes : bytes t
    | Bool : bool t
    | Unit : unit t
    | Option : 'a t -> 'a option t
    | List : 'a t -> 'a list t
    | Array : 'a t -> 'a array t
    | Lazy : 'a t -> 'a lazy_t t
    | Ref : 'a t -> 'a ref t
    | Function : ('dom t * 'rng t) -> ('dom -> 'rng) t
    | Tuple : 'a Typerep.Tuple.t -> 'a t
    | Record : 'a Typerep.Record.t -> 'a t
    | Variant : 'a Typerep.Variant.t -> 'a t
    | Named : ('a Typerep.Named.t * 'a t lazy_t option) -> 'a t

  type packed = T : 'a t -> packed

  module Named = struct
    module type T0 = sig
      type named
      type t
      val witness : (t, named) Type_equal.t
    end
    module type T1 = sig
      type 'a named
      type a val a : a Typerep.t
      type t
      val witness : (t, a named) Type_equal.t
    end
    module type T2 = sig
      type ('a, 'b) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type t
      val witness : (t, (a, b) named) Type_equal.t
    end
    module type T3 = sig
      type ('a, 'b, 'c) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type c val c : c Typerep.t
      type t
      val witness : (t, (a, b, c) named) Type_equal.t
    end
    module type T4 = sig
      type ('a, 'b, 'c, 'd) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type c val c : c Typerep.t
      type d val d : d Typerep.t
      type t
      val witness : (t, (a, b, c, d) named) Type_equal.t
    end
    module type T5 = sig
      type ('a, 'b, 'c, 'd, 'e) named
      type a val a : a Typerep.t
      type b val b : b Typerep.t
      type c val c : c Typerep.t
      type d val d : d Typerep.t
      type e val e : e Typerep.t
      type t
      val witness : (t, (a, b, c, d, e) named) Type_equal.t
    end
    (* there the module is necessary because we need to deal with a type [t] with
       parameters whose kind is not representable as a type variable: ['a 't], even with
       a gadt. *)
    type 'a t =
    | T0 of (module T0 with type t = 'a)
    | T1 of (module T1 with type t = 'a)
    | T2 of (module T2 with type t = 'a)
    | T3 of (module T3 with type t = 'a)
    | T4 of (module T4 with type t = 'a)
    | T5 of (module T5 with type t = 'a)

    let arity = function
      | T0 _ -> 0
      | T1 _ -> 1
      | T2 _ -> 2
      | T3 _ -> 3
      | T4 _ -> 4
      | T5 _ -> 5

  end

  module Tuple = struct
    (* these constructors could be plunged at toplevel of Typerep.t, however it is less
       verbose this way *)
    type _ t =
    | T2 : ('a Typerep.t * 'b Typerep.t)
      -> ('a * 'b) t
    | T3 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t)
      -> ('a * 'b * 'c) t
    | T4 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t)
      -> ('a * 'b * 'c * 'd) t
    | T5 : ('a Typerep.t * 'b Typerep.t * 'c Typerep.t * 'd Typerep.t * 'e Typerep.t)
      -> ('a * 'b * 'c * 'd * 'e) t

    let arity : type a. a t -> int = function
      | Typerep.Tuple.T2 _ -> 2
      | Typerep.Tuple.T3 _ -> 3
      | Typerep.Tuple.T4 _ -> 4
      | Typerep.Tuple.T5 _ -> 5

  end

  include Variant_and_record_intf.M (struct type 'a rep = 'a t type 'a t = 'a rep end)
end
