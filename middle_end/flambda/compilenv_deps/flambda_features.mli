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

[@@@ocaml.warning "+a-30-40-41-42"]

val join_points : unit -> bool
val unbox_along_intra_function_control_flow : unit -> bool
val lift_inconstants : unit -> bool
val backend_cse_at_toplevel : unit -> bool
val cse_depth : unit -> int

module Expert : sig
  val denest_at_toplevel : unit -> bool
  val code_id_and_symbol_scoping_checks : unit -> bool
  val fallback_inlining_heuristic : unit -> bool
end
