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

module TE = Typing_env
module TEE = Typing_env_extension

module For_meet = struct
  let name () = "meet"
  let op () : Meet_or_join_op.t = Meet
  let unknown_is_identity () = true
  let unknown_is_absorbing () = false

  module Make (I : Identifiable.S) = struct
    module Set = struct
      type t = I.Set.t
      let union_or_inter = I.Set.inter
    end
  end

  module String_info = Make (String_info)
  module Target_imm = Make (Target_imm)
  module Float = Make (Float)
  module Int32 = Make (Int32)
  module Int64 = Make (Int64)
  module Targetint = struct
    include Make (Targetint)
    module OCaml = Make (Targetint.OCaml)
  end
  module Closure_id = Make (Closure_id)
  module Var_within_closure = Make (Var_within_closure)
  module Tag = Make (Tag)

  let switch_no_bottom meet _join meet_or_join_env thing1 thing2 =
    meet (Meet_or_join_env.meet_env meet_or_join_env) thing1 thing2

  let switch0 meet _join meet_or_join_env thing1 thing2 =
    meet (Meet_or_join_env.meet_env meet_or_join_env) thing1 thing2

  let switch meet _join meet_or_join_env thing1 thing2 =
    meet (Meet_or_join_env.meet_env meet_or_join_env) thing1 thing2
end

module For_join = struct
  let name () = "join"
  let op () : Meet_or_join_op.t = Join
  let unknown_is_identity () = false
  let unknown_is_absorbing () = true

  module Make (I : Identifiable.S) = struct
    module Set = struct
      type t = I.Set.t
      let union_or_inter = I.Set.union
    end
  end

  module String_info = Make (String_info)
  module Target_imm = Make (Target_imm)
  module Float = Make (Float)
  module Int32 = Make (Int32)
  module Int64 = Make (Int64)
  module Targetint = struct
    include Make (Targetint)
    module OCaml = Make (Targetint.OCaml)
  end
  module Closure_id = Make (Closure_id)
  module Var_within_closure = Make (Var_within_closure)
  module Tag = Make (Tag)

  let switch_no_bottom _meet join meet_or_join_env thing1 thing2 =
    join meet_or_join_env thing1 thing2, TEE.empty ()

  let switch0 _meet join meet_or_join_env thing1 thing2 =
    join meet_or_join_env thing1 thing2

  let switch _meet join meet_or_join_env thing1 thing2 : _ Or_bottom.t =
    Ok (join meet_or_join_env thing1 thing2, TEE.empty ())
end
