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

(** The alpha-equivalence classes of expressions that bind variables. *)
type t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

include Contains_ids.S with type t := t

(** The defining expression of the [Let]. *)
val defining_expr : t -> Named.t

(** Look inside the [Let] by choosing a member of the alpha-equivalence
    class. *)
val pattern_match
   : t
  -> f:(Bindable_let_bound.t -> body:Expr.t -> 'a)
  -> 'a

(** Look inside two [Let]s by choosing members of their alpha-equivalence
    classes, using the same bound variables for both. *)
val pattern_match_pair
   : t
  -> t
  -> f:(Bindable_let_bound.t -> body1:Expr.t -> body2:Expr.t -> 'a)
  -> 'a

val create : Bindable_let_bound.t -> defining_expr:Named.t -> body:Expr.t -> t
