(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


type t = Effects.t * Coeffects.t

let print fmt (eff, coeff) =
  Format.fprintf fmt "%a * %a" Effects.print eff Coeffects.print coeff

let compare (e1, c1) (e2, c2) =
  match Effects.compare e1 e2 with
  | 0 -> Coeffects.compare c1 c2
  | res -> res

(* Some useful constants *)
let pure : t = No_effects, No_coeffects
let all : t = Arbitrary_effects, Has_coeffects
let read : t = No_effects, Has_coeffects

(* Joining effects and coeffects *)
let join (eff1, coeff1) (eff2, coeff2) =
  Effects.join eff1 eff2, Coeffects.join coeff1 coeff2


(* For the purpose of commuting (i.e. there is no duplication),
   generative effects do not count. *)
let has_commuting_effects t =
  match (t: t) with
  | No_effects, _
  | Only_generative_effects _, _ -> false
  | Arbitrary_effects, _ -> true

let has_commuting_coeffects t =
  match (t: t) with
  | _, No_coeffects -> false
  | _, Has_coeffects -> true

let commuting_aux t =
  has_commuting_effects t, has_commuting_coeffects t

(* is_pure, aka commutes with everything *)
let is_pure t =
  not (has_commuting_effects t) &&
  not (has_commuting_coeffects t)

(* Can expression with the given effects and coeffects commute ? *)
let commute ec1 ec2 =
  match commuting_aux ec1, commuting_aux ec2 with
  (* Pure computations commute with everything. *)
  | (false, false), _
  | _, (false, false) -> true
  (* Coeffects can commute. *)
  | (false, true), (false, true) -> true
  (* Effects cannot commute with anything *)
  | (true, _), _
  | _, (true, _) -> false

