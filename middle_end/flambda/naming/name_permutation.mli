(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Handling of permutations upon all kinds of bindable names.

    Unlike [Name_occurrences] this module does not segregate names according
    to where they occur (e.g. in terms or in types). *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val empty : t

val print : Format.formatter -> t -> unit

val is_empty : t -> bool

(** Note that [compose] is not commutative.  The result of
    [compose ~second ~first] is that permutation acting initially like
    [first] then subsequently like [second]. *)
val compose : second:t -> first:t -> t

val add_variable : t -> Variable.t -> Variable.t -> t

val add_fresh_variable : t -> Variable.t -> guaranteed_fresh:Variable.t -> t

val apply_variable : t -> Variable.t -> Variable.t

val apply_variable_set : t -> Variable.Set.t -> Variable.Set.t

val apply_name : t -> Name.t -> Name.t

val add_continuation : t -> Continuation.t -> Continuation.t -> t

val add_fresh_continuation
   : t
  -> Continuation.t
  -> guaranteed_fresh:Continuation.t
  -> t

val apply_continuation : t -> Continuation.t -> Continuation.t
