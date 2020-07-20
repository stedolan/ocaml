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

module CUE = Continuation_uses_env
module DE = Simplify_env_and_result.Downwards_env
module LCS = Simplify_env_and_result.Lifted_constant_state
module R = Simplify_env_and_result.Result
module TE = Flambda_type.Typing_env

type t = {
  denv : DE.t;
  continuation_uses_env : CUE.t;
  r : R.t;
  lifted_constants : LCS.t;
}

let print ppf { denv; continuation_uses_env; r; lifted_constants; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(denv@ %a)@]@ \
      @[<hov 1>(continuation_uses_env@ %a)@]@ \
      @[<hov 1>(r@ %a)@]@ \
      @[<hov 1>(lifted_constant_state@ %a)@]\
      )@]"
    DE.print denv
    CUE.print continuation_uses_env
    R.print r
    LCS.print lifted_constants

let create denv continuation_uses_env r = {
  denv;
  continuation_uses_env;
  r;
  lifted_constants = LCS.empty;
}

let denv t = t.denv

let [@inline always] map_denv t ~f =
  { t with
    denv = f t.denv;
  }

let [@inline always] map_denv2 t ~f =
  let denv, user_data = f t.denv in
  let t =
    { t with
      denv;
    }
  in
  t, user_data

let [@inline always] with_denv t denv =
  { t with
    denv;
  }

let r t = t.r

let [@inline always] map_r t ~f =
  { t with
    r = f t.r;
  }

let [@inline always] with_r t r =
  { t with
    r;
  }

let with_continuation_uses_env t continuation_uses_env =
  { t with
    continuation_uses_env;
  }

let record_continuation_use t cont use_kind ~typing_env_at_use
      ~arg_types =
  let cont_uses_env, id =
    CUE.record_continuation_use t.continuation_uses_env cont use_kind
      ~typing_env_at_use ~arg_types
  in
  with_continuation_uses_env t cont_uses_env, id

let compute_handler_env t ~env_at_fork_plus_params_and_consts
      ~consts_lifted_during_body cont ~params =
  CUE.compute_handler_env t.continuation_uses_env
    ~env_at_fork_plus_params_and_consts ~consts_lifted_during_body
    cont ~params

let num_continuation_uses t cont =
  CUE.num_continuation_uses t.continuation_uses_env cont

let continuation_uses_env t = t.continuation_uses_env

let code_age_relation t =
  TE.code_age_relation (DE.typing_env (denv t))

let with_code_age_relation t code_age_relation =
  let typing_env =
    TE.with_code_age_relation (DE.typing_env (denv t)) code_age_relation
  in
  with_denv t (DE.with_typing_env (denv t) typing_env)

let typing_env t = DE.typing_env (denv t)

let add_variable t var ty =
  with_denv t (DE.add_variable (denv t) var ty)

let extend_typing_environment t env_extension =
  with_denv t (DE.extend_typing_environment (denv t) env_extension)

let get_typing_env_no_more_than_one_use t k =
  CUE.get_typing_env_no_more_than_one_use t.continuation_uses_env k

let add_lifted_constant t const =
  { t with
    lifted_constants = LCS.add t.lifted_constants const;
  }

let add_lifted_constants t constants =
  { t with
    lifted_constants = LCS.union t.lifted_constants constants;
  }

let get_lifted_constants t = t.lifted_constants

let clear_lifted_constants t =
  { t with
    lifted_constants = LCS.empty;
  }

let no_lifted_constants t =
  LCS.is_empty t.lifted_constants

let get_and_clear_lifted_constants t =
  let constants = t.lifted_constants in
  let t = clear_lifted_constants t in
  t, constants

let set_lifted_constants t consts =
  { t with lifted_constants = consts; }
