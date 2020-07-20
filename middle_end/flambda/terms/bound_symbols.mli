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

module Code_and_set_of_closures : sig
  type t = {
    code_ids : Code_id.Set.t;
    closure_symbols : Symbol.t Closure_id.Lmap.t;
    (* CR mshinwell: keep a separate field for the symbols being defined? *)
  }

  val print : Format.formatter -> t -> unit
end

type t =
  | Singleton of Symbol.t
    (** A binding of a single symbol of kind [Value]. *)
  | Sets_of_closures of Code_and_set_of_closures.t list
    (** A recursive binding of possibly multiple sets of closures with
        associated code. All code IDs and symbols named in the
        [Code_and_set_of_closures.t list] are in scope for _all_ associated
        [Static_const.code_and_set_of_closures list] values on the right-hand
        side of the corresponding [Let_symbol] expression. Despite the
        recursive nature of the binding, the elements in the
        [Code_and_set_of_closures.t list] must correspond elementwise to the
        elements in the corresponding
        [Static_const.code_and_set_of_closures list]. *)

val being_defined : t -> Symbol.Set.t

val code_being_defined : t -> Code_id.Set.t

val closure_symbols_being_defined : t -> Symbol.Set.t

val everything_being_defined : t -> Code_id_or_symbol.Set.t

include Expr_std.S with type t := t
include Contains_ids.S with type t := t
