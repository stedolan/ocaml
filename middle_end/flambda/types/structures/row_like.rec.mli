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

(* It would be nicer to have the following elsewhere other than with the
   generic definition of the row-like structure, but unfortunately
   restrictions on the evaluation of recursive modules prevent this, as the
   functor applications produce "undefined recursive module" errors. *)

(* CR mshinwell: tidy up! *)
module For_blocks : sig
  type t

  val create_bottom : unit -> t

  type open_or_closed = Open of Tag.t Or_unknown.t | Closed of Tag.t

  val create
     : field_kind:Flambda_kind.t
    -> field_tys:Type_grammar.t list
    -> open_or_closed
    -> t

  val create_blocks_with_these_tags : field_kind:Flambda_kind.t -> Tag.Set.t -> t

  val create_exactly_multiple
     : field_tys_by_tag:Type_grammar.t list Tag.Map.t
    -> t

  val all_tags : t -> Tag.Set.t Or_unknown.t

  val all_tags_and_sizes : t -> Targetint.OCaml.t Tag.Map.t Or_unknown.t

  val get_singleton : t -> (Tag_and_size.t * Product.Int_indexed.t) option

  val is_bottom : t -> bool

  (** The [Maps_to] value which [meet] returns contains the join of all
      [Maps_to] values in the range of the row-like structure after the meet
      operation has been completed. *)
  include Type_structure_intf.S
    with type t := t
    with type flambda_type := Type_grammar.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type meet_or_join_env := Meet_or_join_env.t
    with type typing_env_extension := Typing_env_extension.t
end

module For_closures_entry_by_set_of_closures_contents : sig
  type t

   val create_exactly
     : Closure_id.t
    -> Set_of_closures_contents.t
    -> Closures_entry.t
    -> t

   val create_at_least
     : Closure_id.t
    -> Set_of_closures_contents.t
    -> Closures_entry.t
    -> t

  val get_singleton
     : t
    -> ((Closure_id.t * Set_of_closures_contents.t) * Closures_entry.t) option

  val map_function_decl_types
     : t
    -> f:(Function_declaration_type.t -> Function_declaration_type.t Or_bottom.t)
    -> t Or_bottom.t

  include Type_structure_intf.S
    with type t := t
    with type flambda_type := Type_grammar.t
    with type meet_env := Meet_env.t
    with type meet_or_join_env := Meet_or_join_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t
end
