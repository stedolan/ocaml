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

module T0 = Name_abstraction.Make_list (Kinded_parameter) (Expr)
(* CR mshinwell: This should use [Bindable_continuation].
   [Exn_continuation] involves extra args, but we never have extra args
   here! *)
module T1 = Name_abstraction.Make (Bindable_exn_continuation) (T0)
module A = Name_abstraction.Make (Bindable_continuation) (T1)

type t = {
  abst : A.t;
  dbg : Debuginfo.t;
  params_arity : Flambda_arity.t;
}

let invariant _env _t = ()

let create ~return_continuation exn_continuation params ~dbg ~body ~my_closure =
  let my_closure = Kinded_parameter.create my_closure K.value in
  let t0 = T0.create (params @ [my_closure]) body in
  let t1 = T1.create exn_continuation t0 in
  let abst = A.create return_continuation t1 in
  { abst; dbg;
    params_arity = Kinded_parameter.List.arity params;
  }

let extract_my_closure params_and_my_closure =
  match List.rev params_and_my_closure with
  | my_closure::params_rev ->
    List.rev params_rev, Kinded_parameter.var my_closure
  | [] -> assert false  (* see [create], above. *)

let pattern_match t ~f =
  A.pattern_match t.abst ~f:(fun return_continuation t1 ->
    T1.pattern_match t1 ~f:(fun exn_continuation t0 ->
      T0.pattern_match t0 ~f:(fun params_and_my_closure body ->
        let params, my_closure = extract_my_closure params_and_my_closure in
        f ~return_continuation exn_continuation params ~body ~my_closure)))

let pattern_match_pair t1 t2 ~f =
  A.pattern_match_pair t1.abst t2.abst ~f:(fun return_continuation t1_1 t1_2 ->
    T1.pattern_match_pair t1_1 t1_2 ~f:(fun exn_continuation t0_1 t0_2 ->
      T0.pattern_match_pair t0_1 t0_2 ~f:(fun params_and_my_closure body1 body2 ->
        let params, my_closure = extract_my_closure params_and_my_closure in
        f ~return_continuation exn_continuation params ~body1 ~body2 ~my_closure)))

let print_with_cache ~cache ppf t =
  pattern_match t
    ~f:(fun ~return_continuation exn_continuation params ~body ~my_closure ->
      let my_closure = Kinded_parameter.create my_closure Flambda_kind.value in
      fprintf ppf
        "@[<hov 1>(@<0>%s@<1>\u{03bb}@<0>%s@[<hov 1>\
         @<1>\u{3008}%a@<1>\u{3009}@<1>\u{300a}%a@<1>\u{300b}\
         %a %a @<0>%s.@<0>%s@]@ %a))@]"
        (Flambda_colours.lambda ())
        (Flambda_colours.normal ())
        Continuation.print return_continuation
        Exn_continuation.print exn_continuation
        Kinded_parameter.List.print params
        Kinded_parameter.print my_closure
        (Flambda_colours.elide ())
        (Flambda_colours.normal ())
        (Expr.print_with_cache ~cache) body)

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let params_arity t = t.params_arity

let apply_name_permutation ({ abst; dbg; params_arity; } as t) perm =
  let abst' = A.apply_name_permutation abst perm in
  if abst == abst' then t
  else { abst = abst'; dbg; params_arity; }

let free_names { abst; params_arity = _; dbg = _; } = A.free_names abst

let debuginfo { dbg; _ } = dbg

let all_ids_for_export { abst; params_arity = _; dbg = _; } =
  A.all_ids_for_export abst

let import import_map { abst; params_arity; dbg; } =
  let abst = A.import import_map abst in
  { abst; params_arity; dbg; }
