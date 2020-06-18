(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(** The names of continuations. *)

type t = private int

include Identifiable.S with type t := t

module Sort : sig
  type t =
    | Normal
    | Return
    | Define_root_symbol
    | Toplevel_return
    | Exn
end

val dummy : t

val create : ?sort:Sort.t -> unit -> t

val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

val to_int : t -> int

val sort : t -> Sort.t

val is_exn : t -> bool

module With_args : sig
  type nonrec t = t * Variable.t list
  include Identifiable.S with type t := t
end
