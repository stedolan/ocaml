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

type t =
  | Unknown of {
      arity : Flambda_arity.t;
      handler : Flambda.Continuation_handler.t option;
    }
  | Unreachable of { arity : Flambda_arity.t; }
  | Inline of {
      arity : Flambda_arity.t;
      handler : Flambda.Continuation_handler.t;
    }

val print : Format.formatter -> t -> unit

val arity : t -> Flambda_arity.t
