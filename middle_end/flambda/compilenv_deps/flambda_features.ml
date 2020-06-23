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

let join_points () = !Clflags.Flambda.join_points
let unbox_along_intra_function_control_flow () =
  !Clflags.Flambda.unbox_along_intra_function_control_flow
let lift_inconstants () = !Clflags.Flambda.lift_inconstants
let backend_cse_at_toplevel () = !Clflags.Flambda.backend_cse_at_toplevel
let cse_depth () = !Clflags.Flambda.cse_depth

module Expert = struct
  let denest_at_toplevel () = !Clflags.Flambda.Expert.denest_at_toplevel
  let code_id_and_symbol_scoping_checks () =
    !Clflags.Flambda.Expert.code_id_and_symbol_scoping_checks
  let fallback_inlining_heuristic () =
    !Clflags.Flambda.Expert.fallback_inlining_heuristic
  let inline_effects_in_cmm () =
    !Clflags.Flambda.Expert.inline_effects_in_cmm
end
