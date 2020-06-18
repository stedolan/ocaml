(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2019 OCamlPro SAS                                    *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Remove mutable variables and replace them with immutable variables
    passed around the control flow as necessary.

    This is similar to an SSA transform---except we don't need the
    optimisations typically required when implementing such transforms, since
    the remainder of Flambda will be able to optimise out unnecessary
    constructs inserted by this pass, and the number of mutable variable
    bindings is typically small.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val run : Ilambda.program -> Ilambda.program
