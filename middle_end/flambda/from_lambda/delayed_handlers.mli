(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helper module for lifting of continuations during closure conversion. *)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda

type t

val empty : t

val add_handler : t -> Continuation.t -> Continuation_handler.t -> t

val find_rev_deps
   : t
  -> Name_occurrences.t
  -> (Continuation_handler.t * Name_occurrences.t) Continuation.Map.t

val all
   : t
  -> (Continuation_handler.t * Name_occurrences.t) Continuation.Map.t

val remove_domain_of_map : t -> _ Continuation.Map.t -> t

val union : t -> t -> t
