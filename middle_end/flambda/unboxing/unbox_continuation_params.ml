(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

(* CR mshinwell: Add a command-line flag. *)
let max_unboxing_depth = 1

type 'a create_use_info_result =
  | Ok of 'a
  | Do_not_unbox_this_parameter

type untag =
  | Untag
  | No_untagging

type project_field_result =
  | Simple of Simple.t
  | Default_behaviour of untag

module type Unboxing_spec = sig
  module Index : sig
    include Identifiable.S
    val to_string : t -> string
  end

  module Info : sig type t end

  module Use_info : sig
    type t

    val create : TE.t -> type_at_use:T.t -> t create_use_info_result
  end

  val var_name : string

  val kind_of_unboxed_field_of_param : Index.t -> K.t

  val unbox_recursively : field_type:T.t -> bool

  val unused_extra_arg : Use_info.t -> Index.t -> Simple.t option

  val make_boxed_value
     : Info.t
    -> param_being_unboxed:KP.t
    -> new_params:KP.t Index.Map.t
    -> fields:T.t Index.Map.t
    -> T.t * TEE.t

  val make_boxed_value_accommodating
     : Info.t
    -> Index.t
    -> index_var:Variable.t
    -> untagged_index_var:Variable.t
    -> T.t

  val project_field
     : Info.t
    -> Use_info.t
    -> block:Simple.t option
    -> index:Index.t
    -> project_field_result
end

type 'a or_aborted =
  | Continue of 'a
  | Aborted

module Make (U : Unboxing_spec) = struct
  module Index = U.Index

  let unbox_one_field_of_one_parameter info ~extra_param ~index
        ~arg_types_by_use_id : _ or_aborted =
    (*
    Format.eprintf "UNBOX: %a\n%!" Index.print index;
    *)
    let param_kind = KP.kind extra_param in
    let field_var = Variable.create "field_at_use" in
    let field_name =
      Name_in_binding_pos.create (Name.var field_var) Name_mode.in_types
    in
    Apply_cont_rewrite_id.Map.fold
      (fun id (typing_env_at_use, arg_type_at_use, use_info)
           (acc : _ or_aborted) ->
        match acc with
        | Aborted ->
          (*
          Format.eprintf "Already aborted\n%!";
          *)
          Aborted
        | Continue (extra_args, field_types_by_id) ->
          let untagged_field_var = Variable.create "untagged_field_at_use" in
          let typing_env_at_use =
            (* CR mshinwell: This definition is not needed unless the index
               needs an untagging operation. *)
            TE.add_definition typing_env_at_use
              (Name_in_binding_pos.var
                (Var_in_binding_pos.create untagged_field_var NM.normal))
              K.naked_immediate
          in
          let shape =
            (* CR mshinwell: Rename, something about "shape" *)
            U.make_boxed_value_accommodating info index ~index_var:field_var
              ~untagged_index_var:untagged_field_var
          in
          let env_extension =
            let result_var =
              Var_in_binding_pos.create field_var Name_mode.normal
            in
            (*
            Format.eprintf "Shape:@ %a@ arg_type_at_use:@ %a@ env:@ %a\n%!"
              T.print shape
              T.print arg_type_at_use
              TE.print typing_env_at_use;
            *)
            T.meet_shape typing_env_at_use arg_type_at_use
              ~shape ~result_var ~result_kind:param_kind
          in
          (*
          Format.eprintf "Env extension from meet is:@ %a\n%!"
            (Or_bottom.print TEE.print) env_extension;
          *)
          let field = Simple.var field_var in
          let block =
            (* CR mshinwell: Also done in [Simplify_toplevel], move to [TE] *)
            match T.get_alias_exn arg_type_at_use with
            | exception Not_found -> None
            | block ->
              match
                TE.get_canonical_simple_exn typing_env_at_use
                  ~min_name_mode:Name_mode.normal
                  block
              with
              | exception Not_found -> None
              | block -> Some block
          in
          let fail () =
            (*
            Format.eprintf "not found\n%!";
            *)
            (* CR mshinwell: This should not be called "unused" *)
            match U.unused_extra_arg use_info index with
            | None ->
              (*
              Format.eprintf "Aborting\n%!";
              *)
              Aborted
            | Some simple ->
              (*
              Format.eprintf "Using placeholder %a\n%!"
                Simple.print simple;
              *)
              let extra_arg : EA.t = Already_in_scope simple in
              let extra_args =
                Apply_cont_rewrite_id.Map.add id extra_arg extra_args
              in
              Continue (extra_args, field_types_by_id)
          in
          match env_extension with
          | Bottom ->
            (* The shape will always match the type of the argument unless
               we are trying to retrieve part of the argument that just
               does not exist (e.g. a field in a variant type that does
               not exist across all non-constant constructors). *)
            (* CR mshinwell: [U.unused_extra_arg] should always return
               [Some] in this case *)
            fail ()
          | Ok env_extension ->
            let typing_env_at_use =
              TE.add_definition typing_env_at_use field_name param_kind
            in
            let typing_env_at_use =
              TE.add_env_extension typing_env_at_use env_extension
            in
            let field_type = T.alias_type_of param_kind field in
            let field_types_by_id =
              Apply_cont_rewrite_id.Map.add id
                (typing_env_at_use, field_type)
                field_types_by_id
            in
            match U.project_field info use_info ~block ~index with
            | Simple simple ->
              let extra_arg : EA.t = Already_in_scope simple in
              let extra_args =
                Apply_cont_rewrite_id.Map.add id extra_arg extra_args
              in
              Continue (extra_args, field_types_by_id)
            | Default_behaviour untag ->
              (* Don't unbox parameters unless, at all use sites, there is a
                 non-irrelevant [Simple] available for the corresponding field
                 of the block. *)
              (* CR-someday pchambart: This shouldn't add another load if
                 there is already one in the list of parameters.  That is,
                   apply_cont k (a, b) a
                   where k x y
                 should become:
                   apply_cont k (a, b) a b
                   where k x y b'
                 not:
                   apply_cont k (a, b) a a b
                   where k x y a' b'
                 mshinwell: We never add any loads now, but might in the future.
              *)
              (*
              Format.eprintf "Getting canonical for field %a: "
                Simple.print field;
              Format.eprintf "Canonical lookup is in:@ %a\n%!"
                TE.print typing_env_at_use;
              *)
              let found_canonical simple untag =
                (*
                Format.eprintf "= %a\n%!" Simple.print simple;
                *)
                let extra_arg : EA.t =
                  match untag with
                  | No_untagging -> Already_in_scope simple
                  | Untag ->
                    New_let_binding (untagged_field_var,
                      Unary (Unbox_number Untagged_immediate, simple))
                in
                let extra_args =
                  Apply_cont_rewrite_id.Map.add id extra_arg extra_args
                in
                Continue (extra_args, field_types_by_id)
              in
              match
                TE.get_canonical_simple_exn typing_env_at_use
                  ~min_name_mode:Name_mode.normal
                  field
              with
              | exception Not_found ->
                begin match untag with
                | No_untagging -> fail ()
                | Untag ->
                  (* If we need an untagged value (such as for constant
                     constructors), then if we couldn't find a [Simple]
                     holding the tagged value (which we can subsequently
                     untag), try finding a [Simple] holding the untagged
                     value. *)
                  let untagged_field = Simple.var untagged_field_var in
                  match
                    TE.get_canonical_simple_exn typing_env_at_use
                      ~min_name_mode:Name_mode.normal
                    untagged_field
                  with
                  | exception Not_found -> fail ()
                  | untagged_simple ->
                    if Simple.equal untagged_simple untagged_field then Aborted
                    else found_canonical untagged_simple No_untagging
                end
              | simple ->
                if Simple.equal simple field then Aborted
                else found_canonical simple untag)
      arg_types_by_use_id
      (Continue (Apply_cont_rewrite_id.Map.empty,
        Apply_cont_rewrite_id.Map.empty))

  let unbox_fields_of_one_parameter info ~new_params
        ~arg_types_by_use_id extra_params_and_args : _ or_aborted =
    let result =
      Index.Map.fold (fun index extra_param acc : _ or_aborted ->
          match acc with
          | Aborted -> Aborted
          | Continue (param_types_rev, all_field_types_by_id_rev,
              extra_params_and_args) ->
            (*
            Format.eprintf "INDEX %a\n%!" Index.print index;
            *)
            let result =
              unbox_one_field_of_one_parameter info ~extra_param ~index
                ~arg_types_by_use_id
            in
            match result with
            | Aborted -> Aborted
            | Continue (extra_args, field_types_by_id) ->
              let param_type =
                T.alias_type_of (KP.kind extra_param) (KP.simple extra_param)
              in
              let all_field_types_by_id_rev =
                field_types_by_id :: all_field_types_by_id_rev
              in
              let extra_params_and_args =
                EPA.add extra_params_and_args ~extra_param ~extra_args
              in
              Continue (Index.Map.add index param_type param_types_rev,
                all_field_types_by_id_rev, extra_params_and_args))
        new_params
        (Continue (Index.Map.empty, [], extra_params_and_args))
    in
    match result with
    | Aborted -> Aborted
    | Continue (param_types, all_field_types_by_id_rev,
        extra_params_and_args) ->
      Continue (param_types, List.rev all_field_types_by_id_rev,
        extra_params_and_args)

  let unbox_one_parameter typing_env ~depth ~arg_types_by_use_id
        ~param_being_unboxed ~param_type extra_params_and_args ~unbox_value
        info indexes =
    let orig_arg_types_by_use_id = arg_types_by_use_id in
    let arg_types_by_use_id =
      Apply_cont_rewrite_id.Map.filter_map arg_types_by_use_id
        ~f:(fun _ (typing_env_at_use, arg_type_at_use) ->
          let use_info =
            U.Use_info.create typing_env_at_use ~type_at_use:arg_type_at_use
          in
          match use_info with
          | Do_not_unbox_this_parameter -> None
          | Ok use_info ->
            Some (typing_env_at_use, arg_type_at_use, use_info))
    in
    let do_not_unbox =
      Apply_cont_rewrite_id.Map.cardinal orig_arg_types_by_use_id
        <> Apply_cont_rewrite_id.Map.cardinal arg_types_by_use_id
    in
    if do_not_unbox then
      typing_env, param_type, extra_params_and_args
    else
      let new_params =
        Index.Set.fold (fun index new_params ->
            let name =
              Format.asprintf "%s_%s" U.var_name (Index.to_string index)
            in
            let var = Variable.create name in
            let param =
              KP.create var (U.kind_of_unboxed_field_of_param index)
            in
            Index.Map.add index param new_params)
          indexes
          Index.Map.empty
      in
      let result =
        unbox_fields_of_one_parameter info ~new_params
          ~arg_types_by_use_id extra_params_and_args
      in
      match result with
      | Aborted -> typing_env, param_type, extra_params_and_args
      | Continue (fields, all_field_types_by_id, extra_params_and_args) ->
        let block_type, env_extension =
          U.make_boxed_value info ~param_being_unboxed ~new_params ~fields
        in
        let typing_env =
          TE.add_definitions_of_params typing_env
            ~params:(Index.Map.data new_params)
        in
        let typing_env = TE.add_env_extension typing_env env_extension in
        match T.meet typing_env block_type param_type with
        | Bottom ->
          Misc.fatal_errorf "[meet] between %a and %a should not have \
            failed.  Typing env:@ %a"
            T.print block_type
            T.print param_type
            TE.print typing_env
        | Ok (param_type, env_extension) ->
          let typing_env = TE.add_env_extension typing_env env_extension in
          assert (Index.Map.cardinal fields =
            List.length all_field_types_by_id);
          let typing_env, extra_params_and_args =
            List.fold_left2
              (fun (typing_env, extra_params_and_args) 
                  field_type field_types_by_id ->
                if not (U.unbox_recursively ~field_type) then
                  typing_env, extra_params_and_args
                else begin
                  let typing_env, _, extra_params_and_args =
                    unbox_value typing_env ~depth:(depth + 1)
                      ~arg_types_by_use_id:field_types_by_id
                      ~param_being_unboxed
                      ~param_type:field_type extra_params_and_args
                  in
                  typing_env, extra_params_and_args
                end)
              (typing_env, extra_params_and_args)
              (Index.Map.data fields) all_field_types_by_id
          in
          typing_env, param_type, extra_params_and_args
end

module Block_of_values_spec : Unboxing_spec
  with module Info = Tag
  with module Index = Targetint.OCaml =
struct
  module Index = Targetint.OCaml
  module Info = Tag

  module Use_info = struct
    type t = unit

    let create _typing_env ~type_at_use:_ : _ create_use_info_result = Ok ()
  end

  let var_name = "unboxed"

  let unbox_recursively ~field_type =
    assert (K.equal (T.kind field_type) K.value);
    true

  let kind_of_unboxed_field_of_param _index = K.value

  let unused_extra_arg _ _index = None

  let make_boxed_value tag ~param_being_unboxed:_ ~new_params:_ ~fields =
    let fields = Index.Map.data fields in
    T.immutable_block ~is_unique:false tag ~field_kind:K.value ~fields,
    TEE.empty ()

  let make_boxed_value_accommodating tag index ~index_var
        ~untagged_index_var:_ =
    T.immutable_block_with_size_at_least ~tag:(Known tag)
      ~n:(Targetint.OCaml.add index Targetint.OCaml.one)
      ~field_kind:K.value
      ~field_n_minus_one:index_var

  let project_field _tag () ~block:_ ~index:_ : project_field_result =
    Default_behaviour No_untagging
end

module Variant : sig
  type t

  val create : T.variant_proof -> t

  val max_size : t -> Targetint.OCaml.t

  val const_ctors : t -> Target_imm.Set.t

  val non_const_ctors_with_sizes : t -> Targetint.OCaml.t Tag.Scannable.Map.t
end = struct
  type t = T.variant_proof

  let create variant = variant

  let const_ctors (t : t) = t.const_ctors
  let non_const_ctors_with_sizes (t : t) = t.non_const_ctors_with_sizes

  let max_size (t : t) =
    Tag.Scannable.Map.fold (fun _tag size max_size ->
        if Targetint.OCaml.compare size max_size > 0 then size
        else max_size)
      t.non_const_ctors_with_sizes
      Targetint.OCaml.zero
end

module Variant_index : sig
  type t =
    | Is_int
    | Const_ctor
    | Tag  (* CR mshinwell: rename to Get_tag *)
    | Field of { index : Targetint.OCaml.t; }

  include Identifiable.S with type t := t

  val to_string : t -> string
end = struct
  type t =
    | Is_int
    | Const_ctor
    | Tag
    | Field of { index : Targetint.OCaml.t; }

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Is_int, Is_int -> 0
      | Const_ctor, Const_ctor -> 0
      | Tag, Tag -> 0
      | Field { index = index1; }, Field { index = index2; } ->
        Targetint.OCaml.compare index1 index2
      | Is_int, _ -> -1
      | _, Is_int -> 1
      | Const_ctor, _ -> -1
      | _, Const_ctor -> 1
      | Tag, _ -> -1
      | _, Tag -> 1

    let equal t1 t2 = (compare t1 t2 = 0)

    let print ppf t =
      match t with
      | Is_int -> Format.pp_print_string ppf "Is_int"
      | Const_ctor -> Format.pp_print_string ppf "Const_ctor"
      | Tag -> Format.pp_print_string ppf "Tag"
      | Field { index; } ->
        Format.fprintf ppf "@[<hov 1>(Field@ %a)@]"
          Targetint.OCaml.print index

    let output _ _ = Misc.fatal_error "Not yet implemented"
    let hash _ = Misc.fatal_error "Not yet implemented"
  end)

  let to_string t =
    match t with
    | Is_int -> "is_int"
    | Const_ctor -> "const_ctor"
    | Tag -> "tag"
    | Field { index; } ->
      Format.asprintf "field%a" Targetint.OCaml.print index
end 

(* CR mshinwell: Presumably this can supercede [Block_of_values_spec]? *)
module Variants_spec : Unboxing_spec
  with module Info = Variant
  with module Index = Variant_index =
struct
  module Index = Variant_index

  module Info = Variant

  module Use_info = struct
    type t =
      | Const_ctor
      | Block of { tag : Tag.t; size : Targetint.OCaml.t; }

    let create typing_env ~type_at_use : _ create_use_info_result =
      (* To avoid having to insert a [Switch], for the moment we only unbox
         when the type at each use is that of either an immediate or a block
         of a unique size, but not any more general variant. *)
      match T.prove_tags_and_sizes typing_env type_at_use with
      | Proved tags_to_sizes ->
        begin match Tag.Map.get_singleton tags_to_sizes with
        | Some (tag, size) -> Ok (Block { tag; size; })
        | None -> Do_not_unbox_this_parameter
        end
(* CR mshinwell: See CR below
        let unique_size =
          Tag.Map.data tags_to_sizes
          |> Targetint.OCaml.Set.of_list
          |> Targetint.OCaml.Set.get_singleton
        in
        begin match unique_size with
        | Some size -> Ok (Block { size; })
        | None -> Do_not_unbox_this_parameter
        end
*)
      | Unknown | Invalid ->
        match T.prove_is_a_tagged_immediate typing_env type_at_use with
        | Proved () -> Ok Const_ctor
        | Unknown | Invalid -> Do_not_unbox_this_parameter
        | Wrong_kind ->
          Misc.fatal_errorf "Type %a should be of kind [Value]"
            T.print type_at_use
  end

  let var_name = "unboxed"

  let unbox_recursively ~field_type:_ = false

  let kind_of_unboxed_field_of_param (index : Index.t) =
    match index with
    | Is_int | Tag | Const_ctor -> K.naked_immediate
    | Field _ -> K.value

  let unused_extra_arg (use_info : Use_info.t) (index : Index.t) =
    match index with
    | Is_int
    | Tag ->
      (* These arguments are filled in later via [project_field]. *)
      Some Simple.untagged_const_zero
    | Const_ctor ->
      begin match use_info with
      | Const_ctor ->
        (* If the argument at the use is known to be a constant constructor,
           but there is no available [Simple] corresponding to it, then we
           cannot unbox. *)
        None
      | Block _ ->
        (* There are no constant constructors in the variant at the use site.
           We provide a dummy value. *)
        Some Simple.untagged_const_zero
      end
    | Field { index; } ->
      begin match use_info with
      | Block { tag = _; size; } ->
        (* If the argument at the use is known to be a block, but the field
           at [index] has no available [Simple] corresponding to it, then we
           cannot unbox. *)
        if Targetint.OCaml.(<) index size then None
        else begin
          (* If the argument at the use is known to be a block, but it has
             fewer fields than the maximum number of fields for the variant,
             then we provide a dummy value. *)
          Some Simple.const_zero
        end
      | Const_ctor ->
        (* There are no blocks in the variant at the use site.  We again
           provide a dummy value. *)
        Some Simple.const_zero
      end

  let make_boxed_value variant ~param_being_unboxed ~new_params ~fields =
    let ty =
      let const_ctors =
        T.alias_type_of K.naked_immediate
          (KP.simple (Index.Map.find Const_ctor new_params))
      in
      let non_const_ctors =
        Tag.Scannable.Map.map (fun size ->
            List.map (fun index ->
                let index : Index.t = Field { index; } in
                match Index.Map.find index fields with
                | exception Not_found ->
                  Misc.fatal_errorf "No field for index %a" Index.print index
                | field -> field)
              (Targetint.OCaml.zero_to_n_minus_one ~n:size))
          (Variant.non_const_ctors_with_sizes variant)
      in
      T.variant ~const_ctors ~non_const_ctors
    in
    let param_being_unboxed = KP.simple param_being_unboxed in
    let is_int_name = KP.name (Index.Map.find Is_int new_params) in
    let get_tag_name = KP.name (Index.Map.find Tag new_params) in
    let is_int = Simple.name is_int_name in
    let get_tag = Simple.name get_tag_name in
    let is_int_prim =
      P.Unary (Is_int, param_being_unboxed)
      |> P.Eligible_for_cse.create_exn
    in
    let get_tag_prim =
      P.Unary (Get_tag, param_being_unboxed)
      |> P.Eligible_for_cse.create_exn
    in
    let env_extension =
      TEE.empty ()
      |> TEE.add_cse ~prim:is_int_prim ~bound_to:is_int
      |> TEE.add_cse ~prim:get_tag_prim ~bound_to:get_tag
    in
    let env_extension =
      TEE.add_or_replace_equation env_extension is_int_name
        (T.is_int_for_scrutinee ~scrutinee:param_being_unboxed)
    in
    let env_extension =
      TEE.add_or_replace_equation env_extension get_tag_name
        (T.get_tag_for_block ~block:param_being_unboxed)
    in
    ty, env_extension

  let make_boxed_value_accommodating _variant (index : Index.t) ~index_var
        ~untagged_index_var =
    match index with
    | Is_int
    | Tag -> T.any_value ()
    | Const_ctor ->
      T.open_variant_from_const_ctors_type
        ~const_ctors:(T.alias_type_of K.naked_immediate (
          Simple.var untagged_index_var))
    | Field { index; } ->
      T.open_variant_from_non_const_ctor_with_size_at_least
        ~n:(Targetint.OCaml.add index Targetint.OCaml.one)
        ~field_n_minus_one:index_var

  (* CR mshinwell: Could extend to handle cases where e.g. it's always a
     constant ctor at a use, but not a unique one, and likewise for blocks
     where the sizes are equal but the tags differ.  This sort of thing will
     necessitate inserting extra operations. *)

  let project_field _variant (use_info : Use_info.t) ~block:_
        ~(index : Index.t) : project_field_result =
    match index with
    | Is_int ->
      let simple =
        match use_info with
        | Const_ctor -> Simple.untagged_const_true
        | Block _ -> Simple.untagged_const_false
      in
      Simple simple
    | Const_ctor ->
      begin match use_info with
      | Const_ctor -> Default_behaviour Untag
      | Block _ -> Simple Simple.untagged_const_zero
      end
    | Tag ->
      begin match use_info with
      | Const_ctor -> Simple Simple.untagged_const_zero
      | Block { tag; size = _; } ->
        Simple (Simple.untagged_const_int (Tag.to_targetint_ocaml tag))
      end
    | Field { index; } ->
      match use_info with
      | Const_ctor -> Simple Simple.const_zero
      | Block { tag = _; size = size_at_use; } ->
        if Targetint.OCaml.compare index size_at_use >= 0 then
          Simple Simple.const_zero
        else
          Default_behaviour No_untagging
end

module Block_of_naked_floats_spec : Unboxing_spec
  with module Info = Tag
  with module Index = Targetint.OCaml =
struct
  module Index = Targetint.OCaml
  module Info = Tag

  module Use_info = struct
    type t = unit

    let create _typing_env ~type_at_use:_ : _ create_use_info_result = Ok ()
  end

  let var_name = "unboxed"

  let unbox_recursively ~field_type =
    assert (K.equal (T.kind field_type) K.naked_float);
    false

  let kind_of_unboxed_field_of_param _index = K.naked_float

  let unused_extra_arg _ _index = None

  let make_boxed_value tag ~param_being_unboxed:_ ~new_params:_ ~fields =
    let fields = Index.Map.data fields in
    T.immutable_block ~is_unique:false tag ~field_kind:K.naked_float ~fields,
    TEE.empty ()

  let make_boxed_value_accommodating tag index ~index_var
        ~untagged_index_var:_ =
    T.immutable_block_with_size_at_least ~tag:(Known tag)
      ~n:(Targetint.OCaml.add index Targetint.OCaml.one)
      ~field_kind:K.naked_float
      ~field_n_minus_one:index_var

  let project_field _tag () ~block:_ ~index:_ : project_field_result =
    Default_behaviour No_untagging
end

module Closures_info = struct
  type t = {
    closure_id : Closure_id.t;
    closures_entry : T.Closures_entry.t;
  }
end

module Closures_spec : Unboxing_spec
  with module Info = Closures_info
  with module Index = Var_within_closure =
struct
  module Index = Var_within_closure
  module Info = Closures_info

  module Use_info = struct
    type t = unit

    let create _typing_env ~type_at_use:_ : _ create_use_info_result = Ok ()
  end

  let var_name = "unboxed_clos_var"

  let unbox_recursively ~field_type =
    assert (K.equal (T.kind field_type) K.value);
    true

  let kind_of_unboxed_field_of_param _index = K.value

  let unused_extra_arg _ _index = None

  let make_boxed_value (info : Info.t) ~param_being_unboxed:_ ~new_params:_
        ~fields:closure_vars =
    let closures_entry = info.closures_entry in
    let all_function_decls_in_set =
      T.Closures_entry.function_decl_types closures_entry
    in
    let all_closures_in_set = T.Closures_entry.closure_types closures_entry in
    let ty =
      T.exactly_this_closure info.closure_id
        ~all_function_decls_in_set
        ~all_closures_in_set
        ~all_closure_vars_in_set:closure_vars
    in
    ty, TEE.empty ()

  let make_boxed_value_accommodating (info : Info.t) closure_var ~index_var
        ~untagged_index_var:_ =
    T.closure_with_at_least_this_closure_var closure_var
      ~this_closure:info.closure_id
      ~closure_element_var:index_var

  let project_field _tag () ~block:_ ~index:_ : project_field_result =
    Default_behaviour No_untagging
end

module Make_unboxed_number_spec (N : sig
  val name : string
  val var_name : string

  val tag : Tag.t

  val unboxed_kind : K.t

  val box : T.t -> T.t
end) = struct
  module Index = Targetint.OCaml
  module Info = Tag

  module Use_info = struct
    type t = unit

    let create _typing_env ~type_at_use:_ : _ create_use_info_result = Ok ()
  end

  let var_name = N.var_name

  let unbox_recursively ~field_type =
    assert (K.equal (T.kind field_type) N.unboxed_kind);
    false

  let kind_of_unboxed_field_of_param _index = N.unboxed_kind

  let unused_extra_arg _ _index = None

  let make_boxed_value tag ~param_being_unboxed:_ ~new_params:_ ~fields =
    assert (Tag.equal tag N.tag);
    let fields = Index.Map.data fields in
    match fields with
    | [field] -> N.box field, TEE.empty ()
    | _ -> Misc.fatal_errorf "Boxed %ss only have one field" N.name

  let make_boxed_value_accommodating _tag index ~index_var
        ~untagged_index_var:_ =
    (* CR mshinwell: Should create the type using the tag too. *)
    if not (Targetint.OCaml.equal index Targetint.OCaml.zero) then begin
       Misc.fatal_errorf "Boxed %ss only have one field" N.name
    end;
    N.box (T.alias_type_of N.unboxed_kind (Simple.var index_var))

  let project_field _tag () ~block:_ ~index:_ : project_field_result =
    Default_behaviour No_untagging
end

module Target_imm_spec : Unboxing_spec
  with module Info = Tag
  with module Index = Targetint.OCaml =
Make_unboxed_number_spec (struct
  let name = "immediate"
  let var_name = "untagged_imm"
  let tag = Tag.zero  (* CR mshinwell: make optional *)
  let unboxed_kind = K.naked_immediate
  let box = T.tag_immediate
end)

module Float_spec : Unboxing_spec
  with module Info = Tag
  with module Index = Targetint.OCaml =
Make_unboxed_number_spec (struct
  let name = "float"
  let var_name = "unboxed_float"
  let tag = Tag.double_tag
  let unboxed_kind = K.naked_float
  let box = T.box_float
end)

module Int32_spec : Unboxing_spec
  with module Info = Tag
  with module Index = Targetint.OCaml =
Make_unboxed_number_spec (struct
  let name = "int32"
  let var_name = "unboxed_int32"
  let tag = Tag.custom_tag
  let unboxed_kind = K.naked_int32
  let box = T.box_int32
end)

module Int64_spec : Unboxing_spec
  with module Info = Tag
  with module Index = Targetint.OCaml =
Make_unboxed_number_spec (struct
  let name = "int64"
  let var_name = "unboxed_int64"
  let tag = Tag.custom_tag
  let unboxed_kind = K.naked_int64
  let box = T.box_int64
end)

module Nativeint_spec : Unboxing_spec
  with module Info = Tag
  with module Index = Targetint.OCaml =
Make_unboxed_number_spec (struct
  let name = "nativeint"
  let var_name = "unboxed_nativeint"
  let tag = Tag.custom_tag
  let unboxed_kind = K.naked_nativeint
  let box = T.box_nativeint
end)

module Blocks_of_values = Make (Block_of_values_spec)
module Blocks_of_naked_floats = Make (Block_of_naked_floats_spec)
module Variants = Make (Variants_spec)
module Closures = Make (Closures_spec)
module Target_imms = Make (Target_imm_spec)
module Floats = Make (Float_spec)
module Int32s = Make (Int32_spec)
module Int64s = Make (Int64_spec)
module Nativeints = Make (Nativeint_spec)

let unboxed_number_decisions = [
  T.prove_is_a_tagged_immediate, Target_imms.unbox_one_parameter, Tag.zero;
  T.prove_is_a_boxed_float, Floats.unbox_one_parameter, Tag.double_tag;
  T.prove_is_a_boxed_int32, Int32s.unbox_one_parameter, Tag.custom_tag;
  T.prove_is_a_boxed_int64, Int64s.unbox_one_parameter, Tag.custom_tag;
  T.prove_is_a_boxed_nativeint, Nativeints.unbox_one_parameter, Tag.custom_tag;
]

let rec make_unboxing_decision typing_env ~depth ~arg_types_by_use_id
      ~param_being_unboxed ~param_type extra_params_and_args =
  if depth > max_unboxing_depth then
    typing_env, param_type, extra_params_and_args
  else
    match T.prove_unique_tag_and_size typing_env param_type with
    | Proved (tag, size) ->
      let indexes =
        let size = Targetint.OCaml.to_int size in
        Targetint.OCaml.Set.of_list
          (List.init size (fun index -> Targetint.OCaml.of_int index))
      in
      (* If the fields have kind [Naked_float] then the [tag] will always
         be [Tag.double_array_tag].  See [Row_like.For_blocks]. *)
      if Tag.equal tag Tag.double_array_tag then
        Blocks_of_naked_floats.unbox_one_parameter typing_env ~depth
          ~arg_types_by_use_id ~param_being_unboxed ~param_type
          extra_params_and_args ~unbox_value:make_unboxing_decision
          tag indexes
      else
        begin match Tag.Scannable.of_tag tag with
        | Some _ ->
          Blocks_of_values.unbox_one_parameter typing_env ~depth
            ~arg_types_by_use_id ~param_being_unboxed ~param_type
            extra_params_and_args ~unbox_value:make_unboxing_decision
            tag indexes
        | None ->
          Misc.fatal_errorf "Block that is not of tag [Double_array_tag] \
              and yet also not scannable:@ %a@ in env:@ %a"
            T.print param_type
            TE.print typing_env
        end
    | Wrong_kind | Invalid | Unknown ->
      match T.prove_variant typing_env param_type with
      | Proved variant ->
        (*
        Format.eprintf "Starting variant unboxing\n%!";
        *)
        let variant = Variant.create variant in
        let fields =
          let size = Targetint.OCaml.to_int (Variant.max_size variant) in
          Variant_index.Set.of_list
            (List.init size (fun index : Variant_index.t ->
              Field { index = Targetint.OCaml.of_int index; }))
        in
        let indexes =
          Variant_index.Set.empty
          |> Variant_index.Set.add Is_int
          |> Variant_index.Set.add Const_ctor
          |> Variant_index.Set.add Tag
          |> Variant_index.Set.union fields
        in
        Variants.unbox_one_parameter typing_env ~depth
          ~arg_types_by_use_id ~param_being_unboxed ~param_type
          extra_params_and_args ~unbox_value:make_unboxing_decision
          variant indexes
      | Invalid | Unknown | Wrong_kind ->
        match T.prove_single_closures_entry' typing_env param_type with
        | Proved (closure_id, closures_entry, Ok _func_decl_type) ->
          let closure_var_types =
            T.Closures_entry.closure_var_types closures_entry
          in
          let info : Closures_spec.Info.t =
            { closure_id;
              closures_entry;
            }
          in
          let closure_vars = Var_within_closure.Map.keys closure_var_types in
          Closures.unbox_one_parameter typing_env ~depth
            ~arg_types_by_use_id ~param_being_unboxed ~param_type
            extra_params_and_args ~unbox_value:make_unboxing_decision
            info closure_vars
        | Proved (_, _, (Unknown | Bottom)) | Wrong_kind | Invalid | Unknown ->
          let rec try_unboxing = function
            | [] -> typing_env, param_type, extra_params_and_args
            | (prover, unboxer, tag) :: decisions ->
              let proof : _ T.proof_allowing_kind_mismatch =
                prover typing_env param_type
              in
              match proof with
              | Proved () ->
                let indexes =
                  Targetint.OCaml.Set.singleton Targetint.OCaml.zero
                in
                unboxer typing_env ~depth ~arg_types_by_use_id
                  ~param_being_unboxed ~param_type extra_params_and_args
                  ~unbox_value:make_unboxing_decision tag indexes
              | Wrong_kind | Invalid | Unknown -> try_unboxing decisions
          in
          try_unboxing unboxed_number_decisions

let make_unboxing_decisions0 typing_env ~arg_types_by_use_id ~params
      ~param_types extra_params_and_args =
  assert (List.compare_lengths params param_types = 0);
  let typing_env, param_types_rev, extra_params_and_args =
    List.fold_left (fun (typing_env, param_types_rev, extra_params_and_args)
              (arg_types_by_use_id, (param, param_type)) ->
        let typing_env, param_type, extra_params_and_args =
          make_unboxing_decision typing_env ~depth:0 ~arg_types_by_use_id
            ~param_being_unboxed:param ~param_type extra_params_and_args
        in
        typing_env, param_type :: param_types_rev, extra_params_and_args)
      (typing_env, [], extra_params_and_args)
      (List.combine arg_types_by_use_id
        (List.combine params param_types))
  in
  let typing_env =
    TE.add_equations_on_params typing_env
      ~params ~param_types:(List.rev param_types_rev)
  in
  (*
  Format.eprintf "Final typing env:@ %a\n%!" TE.print typing_env;
  Format.eprintf "EPA:@ %a\n%!" EPA.print extra_params_and_args;
  *)
  typing_env, extra_params_and_args

let make_unboxing_decisions typing_env ~arg_types_by_use_id ~params
      ~param_types extra_params_and_args =
  if Flambda_features.unbox_along_intra_function_control_flow () then
    make_unboxing_decisions0 typing_env ~arg_types_by_use_id ~params
      ~param_types extra_params_and_args
  else
    typing_env, extra_params_and_args
