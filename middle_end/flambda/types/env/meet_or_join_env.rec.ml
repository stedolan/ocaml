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

[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  | Meet of Meet_env.t
  | Join of {
    central_env : Meet_env.t;
    left_join_env : Typing_env.t;
    right_join_env : Typing_env.t;
  }

let print ppf = function
  | Meet meet_env ->
    Format.fprintf ppf
      "@[<hov 1>(env@ %a)@]"
      Meet_env.print meet_env
  | Join { central_env; left_join_env; right_join_env; } ->
    let join_env name ppf env =
      Format.fprintf ppf "@ @[<hov 1>(%s@ %a)@]@" name
        Typing_env.print env
    in
    Format.fprintf ppf
      "@[<hov 1>(\
       @[<hov 1>(central_env@ %a)@]%a%a)@]"
      Meet_env.print central_env
      (join_env "left_join_env") left_join_env
      (join_env "right_join_env") right_join_env

let create_for_meet meet_env = Meet meet_env

let _ = Meet_or_join_env.print

let create_for_join central_env ~left_env ~right_env =
  Join {
    central_env = Meet_env.create central_env;
    left_join_env = left_env;
    right_join_env = right_env;
  }

let meet_env t =
  match t with
  | Meet meet_env -> meet_env
  | Join { central_env; _ } -> central_env

let target_join_env t =
  match t with
  | Join { central_env; _ } -> Meet_env.env central_env
  | Meet meet_env -> Meet_env.env meet_env

let left_join_env t =
  match t with
  | Join { left_join_env; _ } -> left_join_env
  | Meet meet_env -> Meet_env.env meet_env

let right_join_env t =
  match t with
  | Join { right_join_env; _ } -> right_join_env
  | Meet meet_env -> Meet_env.env meet_env

(* CR mshinwell: fix naming, it's odd at the moment to be using
   [already_meeting]... *)
let now_joining t simple1 simple2 =
  match t with
  | Meet meet_env ->
    Meet (Meet_env.now_meeting meet_env simple1 simple2)
  | Join { central_env; left_join_env; right_join_env; } ->
    Join {
      central_env = Meet_env.now_meeting central_env simple1 simple2;
      left_join_env;
      right_join_env;
    }

let already_joining t simple1 simple2 =
  match t with
  | Meet meet_env ->
    Meet_env.already_meeting meet_env simple1 simple2
  | Join { central_env; _ } ->
    Meet_env.already_meeting central_env simple1 simple2
