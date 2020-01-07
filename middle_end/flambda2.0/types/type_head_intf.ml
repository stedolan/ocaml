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

[@@@ocaml.warning "+a-30-40-41-42"]

module type S = sig
  type type_grammar
  type typing_env
  type typing_env_extension
  type meet_env
  type meet_or_join_env

  include Contains_names.S

  include Contains_ids.S with type t := t

  val print : Format.formatter -> t -> unit

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  module Make_meet_or_join (_ : Lattice_ops_intf.S
    with type meet_env := meet_env
    with type meet_or_join_env := meet_or_join_env
    with type typing_env := typing_env
    with type typing_env_extension := typing_env_extension)
  : sig
    val meet_or_join
       : meet_or_join_env
      -> t
      -> t
      -> (t * typing_env_extension) Or_bottom_or_absorbing.t
  end

  val apply_rec_info : t -> Rec_info.t -> t Or_bottom.t
end
