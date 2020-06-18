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

(** The representation of the alpha-equivalence class of bindings of a list
    of parameters, with associated relations thereon, over the code of a
    continuation handler. *)
type t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

include Contains_ids.S with type t := t

(** Create a value of type [t] given information about a continuation
    handler. *)
val create
   : Kinded_parameter.t list
  -> handler:Expr.t
  -> t

(** Choose a member of the alpha-equivalence class to enable examination
    of the parameters, relations thereon and the code over which they
    are scoped. *)
val pattern_match
   : t
  -> f:(Kinded_parameter.t list
    -> handler:Expr.t
    -> 'a)
  -> 'a

(** Choose members of two bindings' alpha-equivalence classes using the same
    parameters. *)
val pattern_match_pair
   : t
  -> t
  -> f:(Kinded_parameter.t list
    -> handler1:Expr.t
    -> handler2:Expr.t
    -> 'a)
  -> 'a

