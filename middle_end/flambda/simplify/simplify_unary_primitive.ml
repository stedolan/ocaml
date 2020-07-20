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

open! Simplify_import

module A = Number_adjuncts
module Float = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

(* CR mshinwell: [meet] operations should not return types that are already
   known about.  The majority of problems like this have been fixed.
   However it looks like there may be another hanging around somewhere.
   In flambdatest/mlexamples/tuple_stub.ml, the [Select_closure] yields the
   following env extension:

    ((equations
   (Tuple_stub.camlTuple_stub__thd3_2 :
     (Val
      ((known_tags
        {(thd3/0 => (Known ((closures { thd3/0 thd3/1 }) (closure_vars { }))),
          ((function_decls
            {(thd3/0
              (Ok (Inlinable (code_id thd3_0_tuple_stub/2) (param_arity 𝕍)
                   (result_arity 𝕍) (stub true) (dbg ) (inline Default_inline)
                   (is_a_functor false) (recursive Non_recursive) (rec_info ((depth 1) (unroll_to None))))))
             (thd3/1
              (Ok (Inlinable (code_id thd3_0/3) (param_arity 𝕍 ⨯ 𝕍 ⨯ 𝕍)
                   (result_arity 𝕍) (stub false) (dbg tuple_stub.ml:1,9--20)
                   (inline Default_inline) (is_a_functor false) (recursive Non_recursive)
                   (rec_info ((depth 1) (unroll_to None))))))})
           (closure_types ((components_by_index {(thd3/0 (Val (= Tuple_stub.camlTuple_stub__thd3_2))) (thd3/1 (Val (= Tuple_stub.camlTuple_stub__thd3_3)))})))
           (closure_var_types ((components_by_index {})))))}) (other_tags Bottom)))
    unboxed_version/48 : (Val (= Tuple_stub.camlTuple_stub__thd3_3)))))

    All that should be present here is the equation on [unboxed_version].
    The type of the symbol appears to be the same as already known in [dacc].
*)

let simplify_select_closure ~move_from ~move_to
      dacc ~original_term ~arg:closure ~arg_ty:closure_ty ~result_var =
  (*
  Format.eprintf "Select_closure %a -> %a, closure type:@ %a@ dacc:@ %a\n%!"
    Closure_id.print move_from
    Closure_id.print move_to
    T.print closure_ty
    DA.print dacc;
  *)
  let result = Simple.var (Var_in_binding_pos.var result_var) in
  let closures =
    Closure_id.Map.empty
    |> Closure_id.Map.add move_from closure
    |> Closure_id.Map.add move_to result
  in
  Simplify_common.simplify_projection
    dacc ~original_term ~deconstructing:closure_ty
    ~shape:(T.at_least_the_closures_with_ids ~this_closure:move_from closures)
    ~result_var ~result_kind:K.value

let simplify_project_var closure_id closure_element ~min_name_mode dacc
      ~original_term ~arg:_closure ~arg_ty:closure_ty ~result_var =
  let reachable, env_extension, dacc =
    Simplify_common.simplify_projection
      dacc ~original_term ~deconstructing:closure_ty
      ~shape:(T.closure_with_at_least_this_closure_var
                ~this_closure:closure_id
                closure_element
                ~closure_element_var:(Var_in_binding_pos.var result_var))
      ~result_var ~result_kind:K.value
  in
  let reachable, removed_var =
    (* CR-someday mshinwell: Perhaps a more elegant way than this could be
       found.  One possibility is that [simplify_projection] should try to
       return a [Simple] rather than the original term, but that might be
       unnecessary work, since the [Simple] will appear in subsequent places
       by virtue of the typing anyway. *)
    match reachable with
    | Reachable _ ->
      let typing_env =
        TE.add_definition (DA.typing_env dacc)
          (Name_in_binding_pos.var result_var)
          K.value
      in
      let typing_env = TE.add_env_extension typing_env env_extension in
      let name = Name.var (Var_in_binding_pos.var result_var) in
      let ty = TE.find typing_env name (Some (K.value)) in
      begin match T.get_alias_exn ty with
      | exception Not_found -> reachable, false
      | alias ->
        match TE.get_canonical_simple_exn ~min_name_mode typing_env alias with
        | exception Not_found -> reachable, false
        | canonical ->
          let reachable = Reachable.reachable (Named.create_simple canonical) in
          reachable, true
      end
    | Invalid _ -> reachable, false
  in
  let dacc =
    if removed_var then dacc
    else DA.add_use_of_closure_var dacc closure_element
  in
  reachable, env_extension, dacc

let simplify_unbox_number (boxable_number_kind : K.Boxable_number.t)
      dacc ~original_term ~arg ~arg_ty:boxed_number_ty ~result_var =
  let shape, result_kind =
    let result_var = Var_in_binding_pos.var result_var in
    match boxable_number_kind with
    | Naked_float ->
      T.boxed_float_alias_to ~naked_float:result_var, K.naked_float
    | Naked_int32 ->
      T.boxed_int32_alias_to ~naked_int32:result_var, K.naked_int32
    | Naked_int64 ->
      T.boxed_int64_alias_to ~naked_int64:result_var, K.naked_int64
    | Naked_nativeint ->
      T.boxed_nativeint_alias_to ~naked_nativeint:result_var, K.naked_nativeint
    | Untagged_immediate ->
      T.tagged_immediate_alias_to ~naked_immediate:result_var,
        K.naked_immediate
  in
  let reachable, env_extension, dacc =
    Simplify_common.simplify_projection
      dacc ~original_term ~deconstructing:boxed_number_ty
      ~shape ~result_var ~result_kind
  in
  let box_prim : P.t =
    Unary (Box_number boxable_number_kind,
      Simple.var (Var_in_binding_pos.var result_var))
  in
  let env_extension =
    TEE.add_cse env_extension ~prim:(P.Eligible_for_cse.create_exn box_prim)
      ~bound_to:arg
  in
  reachable, env_extension, dacc

let simplify_box_number (boxable_number_kind : K.Boxable_number.t)
      dacc ~original_term ~arg:_ ~arg_ty:naked_number_ty ~result_var =
  (* CR mshinwell: This should check the kind of [naked_number_ty] (or
     the creation functions used below should). *)
  let ty =
    match boxable_number_kind with
    | Naked_float -> T.box_float naked_number_ty
    | Naked_int32 -> T.box_int32 naked_number_ty
    | Naked_int64 -> T.box_int64 naked_number_ty
    | Naked_nativeint -> T.box_nativeint naked_number_ty
    | Untagged_immediate -> T.tag_immediate naked_number_ty
  in
  Reachable.reachable original_term,
    TEE.one_equation (Name.var (Var_in_binding_pos.var result_var)) ty,
    dacc

let simplify_is_int_or_get_tag dacc ~original_term ~scrutinee ~scrutinee_ty:_
      ~result_var ~make_shape =
  (* CR mshinwell: Check [scrutinee_ty] (e.g. its kind)? *)
  let name = Name.var (Var_in_binding_pos.var result_var) in
  let env_extension = TEE.one_equation name (make_shape scrutinee) in
  Reachable.reachable original_term, env_extension, dacc

let simplify_is_int dacc ~original_term ~arg:scrutinee ~arg_ty:scrutinee_ty
      ~result_var =
  simplify_is_int_or_get_tag dacc ~original_term ~scrutinee ~scrutinee_ty
    ~result_var ~make_shape:(fun scrutinee -> T.is_int_for_scrutinee ~scrutinee)

let simplify_get_tag dacc ~original_term ~arg:scrutinee ~arg_ty:scrutinee_ty
      ~result_var =
  simplify_is_int_or_get_tag dacc ~original_term ~scrutinee ~scrutinee_ty
    ~result_var ~make_shape:(fun block -> T.get_tag_for_block ~block)

let simplify_array_length dacc ~original_term ~arg:_ ~arg_ty:array_ty
      ~result_var =
  let result = Simple.var (Var_in_binding_pos.var result_var) in
  Simplify_common.simplify_projection
    dacc ~original_term ~deconstructing:array_ty
    ~shape:(T.array_of_length ~length:(T.alias_type_of K.value result))
    ~result_var ~result_kind:K.value

(* CR-someday mshinwell: Consider whether "string length" should be treated
   like a projection (cf. "array length"). *)
let simplify_string_length dacc ~original_term ~arg:_ ~arg_ty:str_ty
      ~result_var =
  let name = Name.var (Var_in_binding_pos.var result_var) in
  let typing_env = DA.typing_env dacc in
  match T.prove_strings typing_env str_ty with
  | Proved str_infos ->
    if String_info.Set.is_empty str_infos then
      let ty = T.bottom K.naked_immediate in
      Reachable.invalid (), TEE.one_equation name ty, dacc
    else
      begin match String_info.Set.get_singleton str_infos with
      | None ->
        let ty = T.unknown K.naked_immediate in
        Reachable.reachable original_term, TEE.one_equation name ty, dacc
      | Some str ->
        let length = Target_imm.int (String_info.size str) in
        let ty = T.this_naked_immediate length in
        Reachable.reachable original_term, TEE.one_equation name ty, dacc
      end
  | Unknown ->
    let ty = T.unknown K.naked_immediate in
    Reachable.reachable original_term, TEE.one_equation name ty, dacc
  | Invalid ->
    let ty = T.bottom K.naked_immediate in
    Reachable.invalid (), TEE.one_equation name ty, dacc

module Unary_int_arith (I : A.Int_number_kind) = struct
  let simplify (op : P.unary_int_arith_op) dacc ~original_term ~arg:_
        ~arg_ty ~result_var =
    let result = Name.var (Var_in_binding_pos.var result_var) in
    let denv = DA.denv dacc in
    let typing_env = DE.typing_env denv in
    let proof = I.unboxed_prover typing_env arg_ty in
    let result_unknown () =
      let env_extension =
        TEE.one_equation result
          (T.unknown (K.Standard_int_or_float.to_kind I.kind))
      in
      Reachable.reachable original_term, env_extension, dacc
    in
    let result_invalid () =
      let env_extension =
        TEE.one_equation result
          (T.bottom (K.Standard_int_or_float.to_kind I.kind))
      in
      Reachable.reachable original_term, env_extension, dacc
    in
    match proof with
    | Proved ints ->
      assert (not (I.Num.Set.is_empty ints));
      begin match op with
      | Neg ->
        let possible_results = I.Num.Set.map (fun i -> I.Num.neg i) ints in
        let ty = I.these_unboxed possible_results in
        let env_extension = TEE.one_equation result ty in
        Reachable.reachable original_term, env_extension, dacc
      | Swap_byte_endianness ->
        let possible_results =
          I.Num.Set.map (fun i -> I.Num.swap_byte_endianness i) ints
        in
        let ty = I.these_unboxed possible_results in
        let env_extension = TEE.one_equation result ty in
        Reachable.reachable original_term, env_extension, dacc
      end
    | Unknown -> result_unknown ()
    | Invalid -> result_invalid ()
end

module Unary_int_arith_tagged_immediate =
  Unary_int_arith (A.For_tagged_immediates)
module Unary_int_arith_naked_immediate =
  Unary_int_arith (A.For_naked_immediates)
module Unary_int_arith_naked_int32 = Unary_int_arith (A.For_int32s)
module Unary_int_arith_naked_int64 = Unary_int_arith (A.For_int64s)
module Unary_int_arith_naked_nativeint = Unary_int_arith (A.For_nativeints)

module Make_simplify_int_conv (N : A.Number_kind) = struct
  let simplify ~(dst : K.Standard_int_or_float.t) dacc ~original_term
        ~arg:_ ~arg_ty ~result_var =
    let result = Name.var (Var_in_binding_pos.var result_var) in
    let denv = DA.denv dacc in
    let typing_env = DE.typing_env denv in
    if K.Standard_int_or_float.equal N.kind dst then
      let env_extension = TEE.one_equation result arg_ty in
      Reachable.reachable original_term, env_extension, dacc
    else
      let proof = N.unboxed_prover typing_env arg_ty in
      let module Num = N.Num in
      match proof with
      | Proved is ->
        assert (Num.Set.cardinal is > 0);
        begin match dst with
        | Tagged_immediate ->
          let imms =
            Num.Set.fold (fun i imms ->
                Target_imm.Set.add (Num.to_immediate i) imms)
              is
              Target_imm.Set.empty
          in
          let ty = T.these_tagged_immediates imms in
          let env_extension = TEE.one_equation result ty in
          Reachable.reachable original_term, env_extension, dacc
        | Naked_immediate ->
          let imms =
            Num.Set.fold (fun i imms ->
                Target_imm.Set.add (Num.to_immediate i) imms)
              is
              Target_imm.Set.empty
          in
          let ty = T.these_naked_immediates imms in
          let env_extension = TEE.one_equation result ty in
          Reachable.reachable original_term, env_extension, dacc
        | Naked_float ->
          let fs =
            Num.Set.fold (fun i fs -> Float.Set.add (Num.to_naked_float i) fs)
              is
              Float.Set.empty
          in
          let ty = T.these_naked_floats fs in
          let env_extension = TEE.one_equation result ty in
          Reachable.reachable original_term, env_extension, dacc
        | Naked_int32 ->
          let is =
            Num.Set.fold (fun i is -> Int32.Set.add (Num.to_naked_int32 i) is)
              is
              Int32.Set.empty
          in
          let ty = T.these_naked_int32s is in
          let env_extension = TEE.one_equation result ty in
          Reachable.reachable original_term, env_extension, dacc
        | Naked_int64 ->
          let is =
            Num.Set.fold (fun i is -> Int64.Set.add (Num.to_naked_int64 i) is)
              is
              Int64.Set.empty
          in
          let ty = T.these_naked_int64s is in
          let env_extension = TEE.one_equation result ty in
          Reachable.reachable original_term, env_extension, dacc
        | Naked_nativeint ->
          let is =
            Num.Set.fold (fun i is ->
                Targetint.Set.add (Num.to_naked_nativeint i) is)
              is
              Targetint.Set.empty
          in
          let ty = T.these_naked_nativeints is in
          let env_extension = TEE.one_equation result ty in
          Reachable.reachable original_term, env_extension, dacc
        end
      | Unknown ->
        let ty = T.unknown (K.Standard_int_or_float.to_kind dst) in
        let env_extension = TEE.one_equation result ty in
        Reachable.reachable original_term, env_extension, dacc
      | Invalid ->
        let ty = T.bottom (K.Standard_int_or_float.to_kind dst) in
        let env_extension = TEE.one_equation result ty in
        Reachable.reachable original_term, env_extension, dacc
end

module Simplify_int_conv_tagged_immediate =
  Make_simplify_int_conv (A.For_tagged_immediates)
module Simplify_int_conv_naked_immediate =
  Make_simplify_int_conv (A.For_naked_immediates)
module Simplify_int_conv_naked_float = Make_simplify_int_conv (A.For_floats)
module Simplify_int_conv_naked_int32 = Make_simplify_int_conv (A.For_int32s)
module Simplify_int_conv_naked_int64 = Make_simplify_int_conv (A.For_int64s)
module Simplify_int_conv_naked_nativeint =
  Make_simplify_int_conv (A.For_nativeints)

let simplify_boolean_not dacc ~original_term ~arg:_ ~arg_ty ~result_var =
  let result = Name.var (Var_in_binding_pos.var result_var) in
  let denv = DA.denv dacc in
  let typing_env = DE.typing_env denv in
  let proof = T.prove_equals_tagged_immediates typing_env arg_ty in
  let result_invalid () =
    let ty = T.bottom K.value in
    let env_extension = TEE.one_equation result ty in
    Reachable.invalid (), env_extension, dacc
  in
  match proof with
  | Proved imms ->
    let imms_ok =
      Target_imm.Set.for_all (fun imm ->
          Target_imm.equal imm Target_imm.zero
            || Target_imm.equal imm Target_imm.one)
        imms
    in
    if not imms_ok then result_invalid ()
    else
      let imms =
        Target_imm.Set.map (fun imm ->
            if Target_imm.equal imm Target_imm.zero then
              Target_imm.one
            else
              Target_imm.zero)
          imms
      in
      let ty = T.these_tagged_immediates imms in
      let env_extension = TEE.one_equation result ty in
      Reachable.reachable original_term, env_extension, dacc
  | Unknown ->
    (* CR-someday mshinwell: This could say something like (in the type) "when
       the input is 0, the value is 1" and vice-versa. *)
    let ty = T.these_tagged_immediates Target_imm.all_bools in
    let env_extension = TEE.one_equation result ty in
    Reachable.reachable original_term, env_extension, dacc
  | Invalid -> result_invalid ()

let simplify_float_arith_op (op : P.unary_float_arith_op) dacc ~original_term
      ~arg:_ ~arg_ty ~result_var =
  let module F = Numbers.Float_by_bit_pattern in
  let result = Name.var (Var_in_binding_pos.var result_var) in
  let denv = DA.denv dacc in
  let typing_env = DE.typing_env denv in
  let proof = T.prove_naked_floats typing_env arg_ty in
  let result_unknown () =
    let ty = T.unknown K.naked_float in
    let env_extension = TEE.one_equation result ty in
    (* CR mshinwell: If this says [invalid] not [reachable] then a function
       that just returns the negation of its float argument will fail to
       compile.  This may indicate a bug elsewhere. *)
    Reachable.reachable original_term, env_extension, dacc
  in
  let result_invalid () =
    let ty = T.bottom K.naked_float in
    let env_extension = TEE.one_equation result ty in
    Reachable.invalid (), env_extension, dacc
  in
  match proof with
  | Proved fs when DE.float_const_prop denv ->
    assert (not (Float.Set.is_empty fs));
    let possible_results =
      match op with
      | Abs -> F.Set.map (fun f -> F.IEEE_semantics.abs f) fs
      | Neg -> F.Set.map (fun f -> F.IEEE_semantics.neg f) fs
    in
    let ty = T.these_naked_floats possible_results in
    let env_extension = TEE.one_equation result ty in
    Reachable.reachable original_term, env_extension, dacc
  | Proved _ | Unknown -> result_unknown ()
  | Invalid -> result_invalid ()

let try_cse dacc prim arg ~min_name_mode ~result_var : Simplify_common.cse =
  let result_kind = P.result_kind_of_unary_primitive' prim in
  if Name_mode.is_phantom min_name_mode then
    Not_applied dacc
  else
    match S.simplify_simple dacc arg ~min_name_mode with
    | Bottom, _arg_ty -> Invalid (T.bottom result_kind)
    | Ok arg, _arg_ty ->
      let original_prim : P.t = Unary (prim, arg) in
      Simplify_common.try_cse dacc ~original_prim ~result_kind
        ~min_name_mode ~result_var

let simplify_unary_primitive dacc (prim : P.unary_primitive)
      arg dbg ~result_var =
  let min_name_mode = Var_in_binding_pos.name_mode result_var in
  let result_var' = Var_in_binding_pos.var result_var in
  let invalid ty =
    let env_extension = TEE.one_equation (Name.var result_var') ty in
    Reachable.invalid (), env_extension, dacc
  in
  match try_cse dacc prim arg ~min_name_mode ~result_var:result_var' with
  | Invalid ty -> invalid ty
  | Applied result -> result
  | Not_applied dacc ->
    let result_kind = P.result_kind_of_unary_primitive' prim in
    match S.simplify_simple dacc arg ~min_name_mode with
    | Bottom, _arg_ty -> invalid (T.bottom result_kind)
    | Ok arg, arg_ty ->
      let original_prim : P.t = Unary (prim, arg) in
      let original_term = Named.create_prim original_prim dbg in
      let simplifier =
        match prim with
        | Project_var { project_from; var; } ->
          simplify_project_var project_from var ~min_name_mode
        | Select_closure { move_from; move_to; } ->
          simplify_select_closure ~move_from ~move_to
        | Unbox_number boxable_number_kind ->
          simplify_unbox_number boxable_number_kind
        | Box_number boxable_number_kind ->
          simplify_box_number boxable_number_kind
        | Is_int -> simplify_is_int
        | Get_tag -> simplify_get_tag
        | Array_length _ -> simplify_array_length
        | String_length _ -> simplify_string_length
        | Int_arith (kind, op) ->
          begin match kind with
          | Tagged_immediate -> Unary_int_arith_tagged_immediate.simplify op
          | Naked_immediate -> Unary_int_arith_naked_immediate.simplify op
          | Naked_int32 -> Unary_int_arith_naked_int32.simplify op
          | Naked_int64 -> Unary_int_arith_naked_int64.simplify op
          | Naked_nativeint -> Unary_int_arith_naked_nativeint.simplify op
          end
        | Float_arith op -> simplify_float_arith_op op
        | Num_conv { src; dst; } ->
          begin match src with
          | Tagged_immediate -> Simplify_int_conv_tagged_immediate.simplify ~dst
          | Naked_immediate -> Simplify_int_conv_naked_immediate.simplify ~dst
          | Naked_float -> Simplify_int_conv_naked_float.simplify ~dst
          | Naked_int32 -> Simplify_int_conv_naked_int32.simplify ~dst
          | Naked_int64 -> Simplify_int_conv_naked_int64.simplify ~dst
          | Naked_nativeint -> Simplify_int_conv_naked_nativeint.simplify ~dst
          end
        | Boolean_not -> simplify_boolean_not
        | Int_as_pointer
        | Bigarray_length _
        | Duplicate_array _
        | Duplicate_block _
        | Opaque_identity ->
          (* CR mshinwell: In these cases, the type of the argument should
             still be checked.  Same for binary/ternary/etc. *)
          fun dacc ~original_term:_ ~arg ~arg_ty:_ ~result_var:_ ->
            let named = Named.create_prim (Unary (prim, arg)) dbg in
            let ty = T.unknown result_kind in
            let env_extension = TEE.one_equation (Name.var result_var') ty in
            Reachable.reachable named, env_extension, dacc
      in
      simplifier dacc ~original_term ~arg ~arg_ty ~result_var
