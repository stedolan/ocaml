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

module A =
  Name_abstraction.Make_map (Bindable_variable_in_types) (Typing_env_level)

(* The record is here to avoid the double vision problem.  (Otherwise
   there would already be an equality
     t = Name_abstraction.Make_list ...
   meaning that the equality
     t = Typing_env_extension.t
   could not be added by the type checker.) *)
type t = {
  abst : A.t;
} [@@unboxed]

let print ppf { abst; } =
  Name_abstraction.with_printing_style Existential ~f:(fun () ->
    A.print ppf abst)

let invariant { abst; } =
  A.pattern_match abst ~f:(fun level -> Typing_env_level.invariant level)

let empty =
  lazy (
    let abst =
      A.create Bindable_variable_in_types.Map.empty (Typing_env_level.empty ())
    in
    { abst; })

let empty () = Lazy.force empty

let is_empty { abst; } =
  A.pattern_match abst ~f:(fun level -> Typing_env_level.is_empty level)

(* CR mshinwell: It might be worth adding a parameter here that asserts
   whether the [defined_vars] of the level are expected to be empty.  This
   should always be the case for extensions generated from [meet]. *)
let create level =
  let abst =
    A.create (Typing_env_level.defined_vars level) level
  in
  { abst; }

let pattern_match { abst; } ~f =
  A.pattern_match abst ~f:(fun level -> f level)

let one_equation name ty =
  let abst =
    A.create Variable.Map.empty (Typing_env_level.one_equation name ty)
  in
  { abst; }

let add_or_replace_equation { abst; } name ty =
  let abst =
    A.pattern_match abst ~f:(fun level ->
      let level = Typing_env_level.add_or_replace_equation level name ty in
      A.create (Typing_env_level.defined_vars level) level)
  in
  { abst; }

let add_cse { abst; } ~prim ~bound_to =
  let abst =
    A.pattern_match abst ~f:(fun level ->
      let level = Typing_env_level.add_cse level prim ~bound_to in
      A.create (Typing_env_level.defined_vars level) level)
  in
  { abst; }

let meet env (t1 : t) (t2 : t) : t =
  (* CR mshinwell: Add [@inlined] annotations inside [pattern_match] itself
     once a means of suppressing warning 55 has been produced *)
  let [@inline always] f level_1 =
    let [@inline always] f level_2 =
      let level = Typing_env_level.meet env level_1 level_2 in
      A.create (Typing_env_level.defined_vars level) level
    in
    A.pattern_match t2.abst ~f
  in
  let abst = A.pattern_match t1.abst ~f in
  { abst; }

let rec n_way_meet env ts =
  match ts with
  | [] -> empty ()
  | t::ts -> meet env t (n_way_meet env ts)

let n_way_join ~env_at_fork envs_with_extensions ~params : t * _ =
  let abst, extra_cse_bindings =
    let rec open_binders envs_with_extensions envs_with_levels =
      match envs_with_extensions with
      | [] ->
        let level, extra_cse_bindings =
          Typing_env_level.n_way_join ~env_at_fork envs_with_levels ~params
        in
        let abst =
          A.create (Typing_env_level.defined_vars level) level
        in
        abst, extra_cse_bindings
      | (env_at_use, id, use_kind, t)::envs_with_extensions ->
        A.pattern_match t.abst ~f:(fun level ->
          let env =
            Typing_env.add_env_extension_from_level env_at_fork level
          in
          let env =
            Typing_env.with_code_age_relation env
              (Typing_env.code_age_relation env_at_use)
          in
          (* It doesn't matter that the list gets reversed. *)
          let envs_with_levels =
            (env, id, use_kind, level) :: envs_with_levels
          in
          open_binders envs_with_extensions envs_with_levels)
    in
    open_binders envs_with_extensions []
  in
  { abst; }, extra_cse_bindings

let join env ~params ext1 ext2 =
  (* CR mshinwell: This may only be used now when there are no
     variables defined in the extensions *)
  let left_env = Meet_or_join_env.left_join_env env in
  let right_env = Meet_or_join_env.right_join_env env in
  let env_at_fork = Meet_or_join_env.target_join_env env in
  let env_extension, _ =
    n_way_join ~params ~env_at_fork [
      left_env, Apply_cont_rewrite_id.create (), Non_inlinable, ext1;
      right_env, Apply_cont_rewrite_id.create (), Non_inlinable, ext2;
    ]
  in
  env_extension
