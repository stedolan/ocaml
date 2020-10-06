module type S_indexed = sig
  type 'a t
  val ignore_m : unit t
end

module type T = S_indexed with type 'a t := 'a
