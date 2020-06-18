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

module DA = Downwards_acc
module T = Flambda_type
module TE = T.Typing_env

(* CR mshinwell: Is the best place for this?  Are there other places where
   this may be required? *)
(* CR mshinwell: avoid having two functions *)
let type_for_simple simple kind : _ Or_bottom.t =
  let ty = T.alias_type_of kind simple in
  match Simple.rec_info simple with
  | None -> Ok (simple, ty)
  | Some rec_info ->
    match T.apply_rec_info ty rec_info with
    | Bottom -> Bottom
    | Ok ty -> Ok (simple, ty)

let type_for_simple' simple kind : _ Or_bottom.t * _ =
  let ty = T.alias_type_of kind simple in
  match Simple.rec_info simple with
  | None -> Ok simple, ty
  | Some rec_info ->
    match T.apply_rec_info ty rec_info with
    | Bottom -> Bottom, T.bottom (T.kind ty)
    | Ok ty -> Ok simple, ty

let simplify_simple dacc simple ~min_name_mode : _ Or_bottom.t * _ =
  let typing_env = DA.typing_env dacc in
  match
    TE.get_canonical_simple_with_kind_exn typing_env simple ~min_name_mode
  with
  | exception Not_found ->
    Misc.fatal_errorf "No canonical [Simple] for %a exists at the@ \
        requested name mode (%a) or one greater.@ Downwards accumulator:@ %a"
      Simple.print simple
      Name_mode.print min_name_mode
      DA.print dacc
  | simple, kind -> type_for_simple' simple kind

type changed =
  | Unchanged
  | Changed

let simplify_simples dacc simples ~min_name_mode =
  let typing_env = DA.typing_env dacc in
  let changed = ref Unchanged in
  let result =
    Or_bottom.all (List.map (fun simple : _ Or_bottom.t ->
        match
          TE.get_canonical_simple_with_kind_exn typing_env simple
            ~min_name_mode
        with
        | new_simple, kind ->
          if new_simple != simple then begin
            changed := Changed;
          end;
          type_for_simple new_simple kind
        | exception Not_found ->
          Misc.fatal_errorf "No canonical [Simple] for %a exists at the@ \
              requested name mode (%a) or one greater.@ \
              Downwards accumulator:@ %a"
            Simple.print simple
            Name_mode.print min_name_mode
            DA.print dacc)
      simples)
  in
  !changed, result

let simplify_simples' dacc simples ~min_name_mode =
  let typing_env = DA.typing_env dacc in
  let changed = ref Unchanged in
  let result =
    Or_bottom.all (List.map (fun simple : _ Or_bottom.t ->
        match TE.get_canonical_simple_exn typing_env simple ~min_name_mode with
        | new_simple ->
          if new_simple != simple then begin
            changed := Changed;
          end;
          Ok new_simple
        | exception Not_found ->
          Misc.fatal_errorf "No canonical [Simple] for %a exists at the@ \
              requested name mode (%a) or one greater.@ \
              Downwards accumulator:@ %a"
            Simple.print simple
            Name_mode.print min_name_mode
            DA.print dacc)
      simples)
  in
  !changed, result
