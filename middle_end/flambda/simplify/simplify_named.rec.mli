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

(** Simplification of the right-hand sides of [Let] bindings. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type simplify_named_result = private
  | Bindings of {
      bindings_outermost_first : (Bindable_let_bound.t * Reachable.t) list;
      dacc : Downwards_acc.t;
    }
  | Reified of {
      definition : Named.t;
      bound_symbol : Bound_symbols.t;
      symbol : Symbol.t;
      static_const :  Static_const.t;
    }
  | Shared of Symbol.t

val simplify_named
   : Downwards_acc.t
  -> Bindable_let_bound.t
  -> Flambda.Named.t
  -> simplify_named_result
