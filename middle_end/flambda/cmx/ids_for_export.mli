(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = {
  symbols : Symbol.Set.t;
  variables : Variable.Set.t;
  simples : Simple.Set.t;
  consts : Reg_width_things.Const.Set.t;
  code_ids : Code_id.Set.t;
}

val empty : t

val create
   : ?symbols: Symbol.Set.t
  -> ?variables: Variable.Set.t
  -> ?simples: Simple.Set.t
  -> ?consts: Reg_width_things.Const.Set.t
  -> ?code_ids: Code_id.Set.t
  -> unit
  -> t

val from_simple : Simple.t -> t

val add_const : t -> Reg_width_things.Const.t -> t

val add_variable : t -> Variable.t -> t

val add_symbol : t -> Symbol.t -> t

val add_name : t -> Name.t -> t

val add_simple : t -> Simple.t -> t

val add_code_id : t -> Code_id.t -> t

val union : t -> t -> t

module Import_map : sig
  type t

  val create
     : symbols : Symbol.t Symbol.Map.t
    -> variables : Variable.t Variable.Map.t
    -> simples : Simple.t Simple.Map.t
    -> consts : Reg_width_things.Const.t Reg_width_things.Const.Map.t
    -> code_ids : Code_id.t Code_id.Map.t
    -> used_closure_vars : Var_within_closure.Set.t
    -> t

  val const : t -> Reg_width_things.Const.t -> Reg_width_things.Const.t
  val variable : t -> Variable.t -> Variable.t
  val symbol : t -> Symbol.t -> Symbol.t
  val name : t -> Name.t -> Name.t
  val simple : t -> Simple.t -> Simple.t
  val code_id : t -> Code_id.t -> Code_id.t
  val closure_var_is_used : t -> Var_within_closure.t -> bool
end

