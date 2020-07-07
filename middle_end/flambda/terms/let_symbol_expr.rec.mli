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

(** The form of expression that binds symbols to statically-allocated
    constants. *)
module Bound_symbols : sig
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
end

module Scoping_rule : sig
  type t =
    | Syntactic
    | Dominator
end

type t

val create : Scoping_rule.t -> Bound_symbols.t -> Static_const.t -> Expr.t -> t

val scoping_rule : t -> Scoping_rule.t

val bound_symbols : t -> Bound_symbols.t

val defining_expr : t -> Static_const.t

val body : t -> Expr.t

include Expr_std.S with type t := t

include Contains_ids.S with type t := t

(** If [newer_versions_of] maps [id1] to [id2] then [id1] is a newer
    version of [id2]. *)
val pieces_of_code
   : ?newer_versions_of:Code_id.t Code_id.Map.t
  -> ?set_of_closures:(Symbol.t Closure_id.Lmap.t * Set_of_closures.t)
  -> Function_params_and_body.t Code_id.Lmap.t
  -> Bound_symbols.t * Static_const.t

val deleted_pieces_of_code
   : ?newer_versions_of:Code_id.t Code_id.Map.t
  -> Code_id.Set.t
  -> Bound_symbols.t * Static_const.t
