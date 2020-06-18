(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The signature of a module that is used for specialising generic
    meet-and-join operations to either meet or join. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  type typing_env
  type meet_env
  type meet_or_join_env
  type typing_env_extension

  val name : unit -> string

  val op : unit -> Meet_or_join_op.t

  val unknown_is_identity : unit -> bool
  val unknown_is_absorbing : unit -> bool

  module String_info : sig
    module Set : sig
      type t = String_info.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  module Target_imm : sig
    module Set : sig
      type t = Target_imm.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  module Float : sig
    module Set : sig
      type t = Numbers.Float_by_bit_pattern.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  module Int32 : sig
    module Set : sig
      type t = Numbers.Int32.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  module Int64 : sig
    module Set : sig
      type t = Numbers.Int64.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  module Targetint : sig
    module Set : sig
      type t = Targetint.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  val switch_no_bottom
     : (meet_env -> 'a -> 'a -> 'a * typing_env_extension)
    -> (meet_or_join_env -> 'a -> 'a -> 'a)
    -> meet_or_join_env
    -> 'a
    -> 'a
    -> 'a * typing_env_extension

  val switch0
     : (meet_env -> 'a -> 'a -> 'a)
    -> (meet_or_join_env -> 'a -> 'a -> 'a)
    -> meet_or_join_env
    -> 'a
    -> 'a
    -> 'a

  val switch
     : (meet_env -> 'a -> 'a -> ('a * typing_env_extension) Or_bottom.t)
    -> (meet_or_join_env -> 'a -> 'a -> 'a)
    -> meet_or_join_env
    -> 'a
    -> 'a
    -> ('a * typing_env_extension) Or_bottom.t
end
