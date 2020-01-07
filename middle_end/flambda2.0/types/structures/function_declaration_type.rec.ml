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

module TE = Typing_env
module TEE = Typing_env_extension

module Inlinable = struct
  type t = {
    code_id : Code_id.t;
    param_arity : Flambda_arity.t;
    result_arity : Flambda_arity.t;
    stub : bool;
    dbg : Debuginfo.t;
    inline : Inline_attribute.t;
    is_a_functor : bool;
    recursive : Recursive.t;
    rec_info : Rec_info.t;
  }

  let print ppf { code_id; param_arity; result_arity; stub; dbg;
                  inline; is_a_functor; recursive; rec_info; } =
    Format.fprintf ppf
      "@[<hov 1>(Inlinable@ \
        @[<hov 1>(code_id@ %a)@]@ \
        @[<hov 1>(param_arity@ %a)@]@ \
        @[<hov 1>(result_arity@ %a)@] \
        @[<hov 1>(stub@ %b)@] \
        @[<hov 1>(dbg@ %a)@] \
        @[<hov 1>(inline@ %a)@] \
        @[<hov 1>(is_a_functor@ %b)@] \
        @[<hov 1>(recursive@ %a)@]@ \
        @[<hov 1>(rec_info@ %a)@]\
        )@]"
      Code_id.print code_id
      Flambda_arity.print param_arity
      Flambda_arity.print result_arity
      stub
      Debuginfo.print_compact dbg
      Inline_attribute.print inline
      is_a_functor
      Recursive.print recursive
      Rec_info.print rec_info

  let create ~code_id ~param_arity ~result_arity ~stub ~dbg ~inline
        ~is_a_functor ~recursive ~rec_info =
    { code_id;
      param_arity;
      result_arity;
      stub;
      dbg;
      inline;
      is_a_functor;
      recursive;
      rec_info;
    }

  let code_id t = t.code_id
  let param_arity t = t.param_arity
  let result_arity t = t.result_arity
  let stub t = t.stub
  let dbg t = t.dbg
  let inline t = t.inline
  let is_a_functor t = t.is_a_functor
  let recursive t = t.recursive
  let rec_info t = t.rec_info
end

module Non_inlinable = struct
  type t = {
    code_id : Code_id.t;
    param_arity : Flambda_arity.t;
    result_arity : Flambda_arity.t;
    recursive : Recursive.t;
  }

  let print ppf { code_id; param_arity; result_arity; recursive; } =
    Format.fprintf ppf
      "@[<hov 1>(Non_inlinable@ \
        @[<hov 1>(code_id@ %a)@]@ \
        @[<hov 1>(param_arity@ %a)@]@ \
        @[<hov 1>(result_arity@ %a)@] \
        @[<hov 1>(recursive@ %a)@]\
        )@]"
      Code_id.print code_id
      Flambda_arity.print param_arity
      Flambda_arity.print result_arity
      Recursive.print recursive

  let create ~code_id ~param_arity ~result_arity ~recursive =
    { code_id;
      param_arity;
      result_arity;
      recursive;
    }

  let code_id t = t.code_id
  let param_arity t = t.param_arity
  let result_arity t = t.result_arity
  let recursive t = t.recursive
end

type t0 =
  | Inlinable of Inlinable.t
  | Non_inlinable of Non_inlinable.t

type t = t0 Or_unknown_or_bottom.t

let print_t0 ppf t0 =
  match t0 with
  | Inlinable inlinable -> Inlinable.print ppf inlinable
  | Non_inlinable non_inlinable -> Non_inlinable.print ppf non_inlinable

let print_with_cache ~cache:_ ppf t =
  Or_unknown_or_bottom.print print_t0 ppf t

let print ppf t =
  Or_unknown_or_bottom.print print_t0 ppf t

let free_names (t : t) =
  match t with
  | Bottom | Unknown -> Name_occurrences.empty
  | Ok (Inlinable { code_id; param_arity = _; result_arity = _; stub = _;
                    dbg = _; inline = _; is_a_functor = _; recursive = _;
                    rec_info = _; })
  | Ok (Non_inlinable { code_id; param_arity = _; result_arity = _;
                        recursive = _; }) ->
    Name_occurrences.add_code_id Name_occurrences.empty code_id
      Name_mode.in_types

let all_ids_for_export (t : t) =
  match t with
  | Bottom | Unknown -> Ids_for_export.empty
  | Ok (Inlinable { code_id; param_arity = _; result_arity = _; stub = _;
                    dbg = _; inline = _; is_a_functor = _; recursive = _;
                    rec_info = _; })
  | Ok (Non_inlinable { code_id; param_arity = _; result_arity = _;
                        recursive = _; }) ->
    Ids_for_export.add_code_id Ids_for_export.empty code_id

let import import_map (t : t) : t =
  match t with
  | Bottom | Unknown -> t
  | Ok (Inlinable { code_id; param_arity; result_arity; stub;
                    dbg; inline; is_a_functor; recursive; rec_info; }) ->
    let code_id = Ids_for_export.Import_map.code_id import_map code_id in
    Ok (Inlinable { code_id; param_arity; result_arity; stub;
                    dbg; inline; is_a_functor; recursive; rec_info; })
  | Ok (Non_inlinable { code_id; param_arity; result_arity; recursive; }) ->
    let code_id = Ids_for_export.Import_map.code_id import_map code_id in
    Ok (Non_inlinable { code_id; param_arity; result_arity; recursive; })

let apply_name_permutation t _perm = t

module Make_meet_or_join
  (E : Lattice_ops_intf.S
   with type meet_env := Meet_env.t
   with type meet_or_join_env := Meet_or_join_env.t
   with type typing_env := Typing_env.t
   with type typing_env_extension := Typing_env_extension.t) =
struct
  let meet_or_join (env : Meet_or_join_env.t) (t1 : t) (t2 : t)
        : (t * TEE.t) Or_bottom.t =
    match t1, t2 with
    (* CR mshinwell: Try to factor out "Or_unknown_or_bottom" handling from here
       and elsewhere *)
    | Bottom, Bottom -> Ok (Bottom, TEE.empty ())
    | Bottom, _ ->
      begin match E.op () with
      | Meet -> Ok (Bottom, TEE.empty ())
      | Join -> Ok (t2, TEE.empty ())
      end
    | _, Bottom ->
      begin match E.op () with
      | Meet -> Ok (Bottom, TEE.empty ())
      | Join -> Ok (t1, TEE.empty ())
      end
    | Unknown, Unknown -> Ok (Unknown, TEE.empty ())
    | Unknown, _ ->
      begin match E.op () with
      | Meet -> Ok (t2, TEE.empty ())
      | Join -> Ok (Unknown, TEE.empty ())
      end
    | _, Unknown ->
      begin match E.op () with
      | Meet -> Ok (t1, TEE.empty ())
      | Join -> Ok (Unknown, TEE.empty ())
      end
    | Ok (Non_inlinable {
        code_id = code_id1;
        param_arity = param_arity1; result_arity = result_arity1;
        recursive = recursive1;
      }), Ok (Non_inlinable {
        code_id = code_id2;
        param_arity = param_arity2; result_arity = result_arity2;
        recursive = recursive2;
      }) ->
      let typing_env = Meet_or_join_env.target_join_env env in
      let target_code_age_rel = TE.code_age_relation typing_env in
      let resolver = TE.code_age_relation_resolver typing_env in
      let check_other_things_and_return code_id : (t * TEE.t) Or_bottom.t =
        assert (Flambda_arity.equal param_arity1 param_arity2);
        assert (Flambda_arity.equal result_arity1 result_arity2);
        assert (Recursive.equal recursive1 recursive2);
        Ok (Ok (Non_inlinable {
            code_id;
            param_arity = param_arity1;
            result_arity = result_arity1;
            recursive = recursive1;
          }),
          TEE.empty ())
      in
      begin match E.op () with
      | Meet ->
        begin match
          Code_age_relation.meet target_code_age_rel ~resolver code_id1 code_id2
        with
        | Ok code_id -> check_other_things_and_return code_id
        | Bottom -> Bottom
        end
      | Join ->
        let code_age_rel1 =
          TE.code_age_relation (Meet_or_join_env.left_join_env env)
        in
        let code_age_rel2 =
          TE.code_age_relation (Meet_or_join_env.right_join_env env)
        in
        begin match
          Code_age_relation.join ~target_t:target_code_age_rel ~resolver
            code_age_rel1 code_age_rel2 code_id1 code_id2
        with
        | Known code_id -> check_other_things_and_return code_id
        | Unknown -> Ok (Unknown, TEE.empty ())
        end
      end
    | Ok (Non_inlinable _), Ok (Inlinable _)
    | Ok (Inlinable _), Ok (Non_inlinable _) ->
      (* CR mshinwell: This should presumably return [Non_inlinable] if
         the arities match. *)
      Ok (Unknown, TEE.empty ())
    | Ok (Inlinable {
        code_id = code_id1;
        param_arity = param_arity1;
        result_arity = result_arity1;
        stub = stub1;
        dbg = dbg1;
        inline = inline1;
        is_a_functor = is_a_functor1;
        recursive = recursive1;
        rec_info = _rec_info1;
      }),
      Ok (Inlinable {
        code_id = code_id2;
        param_arity = param_arity2;
        result_arity = result_arity2;
        stub = stub2;
        dbg = dbg2;
        inline = inline2;
        is_a_functor = is_a_functor2;
        recursive = recursive2;
        rec_info = _rec_info2;
      }) ->
      let typing_env = Meet_or_join_env.target_join_env env in
      let target_code_age_rel = TE.code_age_relation typing_env in
      let resolver = TE.code_age_relation_resolver typing_env in
      let check_other_things_and_return code_id : (t * TEE.t) Or_bottom.t =
        assert (Flambda_arity.equal param_arity1 param_arity2);
        assert (Flambda_arity.equal result_arity1 result_arity2);
        assert (Bool.equal stub1 stub2);
        assert (Int.equal (Debuginfo.compare dbg1 dbg2) 0);
        assert (Inline_attribute.equal inline1 inline2);
        assert (Bool.equal is_a_functor1 is_a_functor2);
        assert (Recursive.equal recursive1 recursive2);
        Ok (Ok (Inlinable {
            code_id;
            param_arity = param_arity1;
            result_arity = result_arity1;
            stub = stub1;
            dbg = dbg1;
            inline = inline1;
            is_a_functor = is_a_functor1;
            recursive = recursive1;
            rec_info = _rec_info1;
          }),
          TEE.empty ())
      in
      (* CR mshinwell: What about [rec_info]? *)
      match E.op () with
      | Meet ->
        begin match
          Code_age_relation.meet target_code_age_rel ~resolver code_id1 code_id2
        with
        | Ok code_id -> check_other_things_and_return code_id
        | Bottom -> Bottom
        end
      | Join ->
        let code_age_rel1 =
          TE.code_age_relation (Meet_or_join_env.left_join_env env)
        in
        let code_age_rel2 =
          TE.code_age_relation (Meet_or_join_env.right_join_env env)
        in
        begin match
          Code_age_relation.join ~target_t:target_code_age_rel ~resolver
            code_age_rel1 code_age_rel2 code_id1 code_id2
        with
        | Known code_id -> check_other_things_and_return code_id
        | Unknown -> Ok (Unknown, TEE.empty ())
        end
end

module Meet = Make_meet_or_join (Lattice_ops.For_meet)
module Join = Make_meet_or_join (Lattice_ops.For_join)

let meet env t1 t2 : _ Or_bottom.t =
  let env = Meet_or_join_env.create_for_meet env in
  match Meet.meet_or_join env t1 t2 with
  | Bottom | Ok (Bottom, _) -> Bottom
  | Ok ((Ok _ | Unknown) as t, env_extension) -> Ok (t, env_extension)

let join env t1 t2 : t =
(*
  Format.eprintf "FDT.join:@ %a@ and@ %a@ in:@ %a\n%!"
    print t1 print t2
    Meet_or_join_env.print env;
    *)
  match Join.meet_or_join env t1 t2 with
  | Bottom | Ok (Bottom, _) -> Bottom
  | Ok ((Ok _ | Unknown) as t, env_extension) ->
    assert (TEE.is_empty env_extension);
    t

let apply_rec_info (t : t) rec_info : t Or_bottom.t =
  match t with
  | Ok (Inlinable { code_id; param_arity; result_arity; stub; dbg; inline;
                    is_a_functor; recursive; rec_info = rec_info'; }) ->
    let rec_info = Rec_info.merge rec_info' ~newer:rec_info in
    Ok (Ok (Inlinable { code_id;
      param_arity;
      result_arity;
      stub;
      dbg;
      inline;
      is_a_functor;
      recursive;
      rec_info;
    }))
  | Ok (Non_inlinable { code_id = _; param_arity = _; result_arity = _;
                        recursive = _; }) -> Ok t
  | Unknown | Bottom -> Ok t
