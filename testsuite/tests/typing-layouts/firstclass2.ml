module M (X : sig type 'a t constraint 'a = float end) = struct
  module type S = sig
    type t = float
    val compute : t X.t
  end
end

include M (struct type 'a t = int constraint 'a = float end)

module F (X : S) = struct let foo = X.compute end


(*
module M (X : sig type 'a t end) = struct
  module type S = sig
    type t
    val compute : t X.t
  end
end

include M (struct type 'a t = int end)

module F (X : S) = struct let foo = X.compute end

*)
