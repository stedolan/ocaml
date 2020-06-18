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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  type typing_env
  type meet_env
  type meet_or_join_env
  type typing_env_extension
  type flambda_type

  type t

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val print : Format.formatter -> t -> unit

  (* CR mshinwell: Add [bottom] here?  Probably [is_bottom] too *)

  val meet : meet_env -> t -> t -> (t * typing_env_extension) Or_bottom.t

  (* CR mshinwell: The signature of [join] implies that each [t] must have
     a bottom element in itself.  How do we reconcile that against the fact
     that we're trying to propagate bottom upwards? *)
  val join : meet_or_join_env -> t -> t -> t

  include Contains_names.S with type t := t

  include Contains_ids.S with type t := t
end
