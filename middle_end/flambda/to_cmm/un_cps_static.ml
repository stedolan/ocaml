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

[@@@ocaml.warning "+a-4-30-40-41-42"]

[@@@ocaml.warning "-27"] (* FIXME remove this once closures changes done *)
[@@@ocaml.warning "-32"] (* FIXME remove this once closures changes done *)

open! Flambda.Import

module C = struct
  include Cmm_helpers
  include Un_cps_helper
end

module Bound_symbols = Let_symbol.Bound_symbols
module Env = Un_cps_env
module SC = Flambda.Static_const
module R = Un_cps_result
module BSCSC = Bound_symbols.Code_and_set_of_closures
module SCCSC = Static_const.Code_and_set_of_closures

(* CR mshinwell: Share these next functions with Un_cps.  Unfortunately
   there's a name clash with at least one of them ("symbol") with functions
   already in Un_cps_helper. *)
let symbol s =
  Linkage_name.to_string (Symbol.linkage_name s)

let tag_targetint t = Targetint.(add (shift_left t 1) one)

let targetint_of_imm i = Targetint.OCaml.to_targetint i.Target_imm.value

let nativeint_of_targetint t =
  match Targetint.repr t with
  | Int32 i -> Nativeint.of_int32 i
  | Int64 i -> Int64.to_nativeint i

let filter_closure_vars env s =
  let used_closure_vars = Env.used_closure_vars env in
  let aux clos_var _bound_to =
    Var_within_closure.Set.mem clos_var used_closure_vars
  in
  Var_within_closure.Map.filter aux s

let todo () = failwith "Not yet implemented"
(* ----- End of functions to share ----- *)

let name_static env name =
  Name.pattern_match name
    ~var:(fun v -> env, `Var v)
    ~symbol:(fun s ->
      Env.check_scope ~allow_deleted:false env (Code_id_or_symbol.Symbol s),
      `Data [C.symbol_address (symbol s)])

let const_static _env cst =
  match Reg_width_const.descr cst with
  | Naked_immediate i ->
      [C.cint (nativeint_of_targetint (targetint_of_imm i))]
  | Tagged_immediate i ->
      [C.cint (nativeint_of_targetint (tag_targetint (targetint_of_imm i)))]
  | Naked_float f ->
      [C.cfloat (Numbers.Float_by_bit_pattern.to_float f)]
  | Naked_int32 i ->
      [C.cint (Nativeint.of_int32 i)]
  | Naked_int64 i ->
      if C.arch32 then todo() (* split int64 on 32-bit archs *)
      else [C.cint (Int64.to_nativeint i)]
  | Naked_nativeint t ->
      [C.cint (nativeint_of_targetint t)]

let simple_static env s =
  Simple.pattern_match s
    ~name:(fun n -> name_static env n)
    ~const:(fun c -> env, `Data (const_static env c))

let static_value env v =
  match (v : SC.Field_of_block.t) with
  | Symbol s ->
      Env.check_scope ~allow_deleted:false env (Code_id_or_symbol.Symbol s),
      C.symbol_address (symbol s)
  | Dynamically_computed _ -> env, C.cint 1n
  | Tagged_immediate i ->
      env, C.cint (nativeint_of_targetint (tag_targetint (targetint_of_imm i)))

let or_variable f default v cont =
  match (v : _ Or_variable.t) with
  | Const c -> f c cont
  | Var _ -> f default cont

let make_update env kind symb var i prev_update =
  let e = Env.get_variable env var in
  let address = C.field_address symb i Debuginfo.none in
  let update = C.store kind Lambda.Root_initialization address e in
  match prev_update with
  | None -> Some update
  | Some prev_update -> Some (C.sequence prev_update update)

let rec static_block_updates symb env acc i = function
  | [] -> acc
  | sv :: r ->
      begin match (sv : SC.Field_of_block.t) with
      | Symbol _
      | Tagged_immediate _ ->
          static_block_updates symb env acc (i + 1) r
      | Dynamically_computed var ->
          let acc = make_update env Cmm.Word_val symb var i acc in
          static_block_updates symb env acc (i + 1) r
      end

let rec static_float_array_updates symb env acc i = function
  | [] -> acc
  | sv :: r ->
      begin match (sv : _ Or_variable.t) with
      | Const _ ->
          static_float_array_updates symb env acc (i + 1) r
      | Var var ->
          let acc = make_update env Cmm.Double_u symb var i acc in
          static_float_array_updates symb env acc (i + 1) r
      end

let static_boxed_number kind env s default emit transl v r =
  let name = symbol s in
  let aux x cont = emit (name, Cmmgen_state.Global) (transl x) cont in
  let updates =
    match (v : _ Or_variable.t) with
    | Const _ -> None
    | Var v ->
        make_update env kind (C.symbol name) v 0 None
  in
  R.update_data r (or_variable aux default v), updates

let get_whole_closure_symbol =
  let whole_closure_symb_count = ref 0 in
  (fun r ->
     match !r with
     | Some s -> s
     | None ->
         incr whole_closure_symb_count;
         let comp_unit = Compilation_unit.get_current_exn () in
         let linkage_name =
           Linkage_name.create @@
           Printf.sprintf ".clos_%d" !whole_closure_symb_count
         in
         let s = Symbol.create comp_unit linkage_name in
         r := Some s;
         s
  )

let rec static_set_of_closures env symbs set prev_update =
  let clos_symb = ref None in
  let fun_decls = Set_of_closures.function_decls set in
  let decls = Function_declarations.funs fun_decls in
  let elts = filter_closure_vars env (Set_of_closures.closure_elements set) in
  let layout = Env.layout env
      (List.map fst (Closure_id.Map.bindings decls))
      (List.map fst (Var_within_closure.Map.bindings elts))
  in
  let env, l, updates, length =
    fill_static_layout clos_symb symbs decls elts env [] prev_update 0 layout
  in
  let block =
    match l with
    (* Closures can be deleted by flambda but still appear in let_symbols,
       hence we may end up with empty sets of closures. *)
    | [] -> []
    (* Regular case. *)
    | _ ->
      let header = C.cint (C.black_closure_header length) in
      let sdef = match !clos_symb with
        | None -> []
        | Some s -> C.define_symbol ~global:false (symbol s)
      in
      header :: sdef @ l
  in
  env, block, updates

and fill_static_layout s symbs decls elts env acc updates i = function
  | [] -> env, List.rev acc, updates, i
  | (j, slot) :: r ->
      let acc = fill_static_up_to j acc i in
      let env, acc, offset, updates =
        fill_static_slot s symbs decls elts env acc j updates slot
      in
      fill_static_layout s symbs decls elts env acc updates offset r

and fill_static_slot s symbs decls elts env acc offset updates slot =
  match (slot : Un_cps_closure.layout_slot) with
  | Infix_header ->
      let field = C.cint (C.infix_header (offset + 1)) in
      env, field :: acc, offset + 1, updates
  | Env_var v ->
      let env, contents =
        simple_static env (Var_within_closure.Map.find v elts)
      in
      let fields, updates =
        match contents with
        | `Data fields -> fields, updates
        | `Var v ->
            let s = get_whole_closure_symbol s in
            let updates =
              make_update env Cmm.Word_val (C.symbol (symbol s)) v offset updates
            in
            [C.cint 1n], updates
      in
      env, List.rev fields @ acc, offset + 1, updates
  | Closure c ->
      let decl = Closure_id.Map.find c decls in
      let symb = Closure_id.Map.find c symbs in
      let external_name = symbol symb in
      let code_id = Function_declaration.code_id decl in
      let code_symbol = Code_id.code_symbol code_id in
      let code_name = Linkage_name.to_string (Symbol.linkage_name code_symbol) in
      let acc = List.rev (C.define_symbol ~global:true external_name) @ acc in
      let arity = List.length (Function_declaration.params_arity decl) in
      let tagged_arity = arity * 2 + 1 in
      (* We build here the **reverse** list of fields for the closure *)
      if arity = 1 || arity = 0 then begin
        let acc =
          C.cint (Nativeint.of_int tagged_arity) ::
          C.symbol_address code_name ::
          acc
        in
        env, acc, offset + 2, updates
      end else begin
        let acc =
          C.symbol_address code_name ::
          C.cint (Nativeint.of_int tagged_arity) ::
          C.symbol_address (C.curry_function_sym arity) ::
          acc
        in
        env, acc, offset + 3, updates
      end

and fill_static_up_to j acc i =
  if i = j then acc
  else fill_static_up_to j (C.cint 1n :: acc) (i + 1)

let update_env_for_function
    env code_id ~return_continuation:_ _exn_k _ps ~body ~my_closure =
  let free_vars = Expr.free_names body in
  (* Format.eprintf "Free vars: %a@." Variable.Set.print free_vars; *)
  let needs_closure_arg = Name_occurrences.mem_var free_vars my_closure in
  let info : Env.function_info = { needs_closure_arg; } in
  Env.add_function_info env code_id info

let update_env_for_set_of_closure env { SCCSC.code; set_of_closures = _; } =
  Code_id.Map.fold
    (fun code_id SC.Code.({ params_and_body = p; newer_version_of; }) env ->
       (* Check scope of the closure id *)
       let env =
         match newer_version_of with
         | None -> env
         | Some code_id ->
           Env.check_scope ~allow_deleted:true env
             (Code_id_or_symbol.Code_id code_id)
       in
       match (p : _ SC.Code.or_deleted) with
       | Deleted ->
         Env.mark_code_id_as_deleted env code_id
       | Present p ->
         Function_params_and_body.pattern_match p
           ~f:(update_env_for_function env code_id)
    ) code env

let add_function env r ~params_and_body code_id p =
  let fun_symbol = Code_id.code_symbol code_id in
  let fun_name =
    Linkage_name.to_string (Symbol.linkage_name fun_symbol)
  in
  let fundecl, r = params_and_body env r fun_name p in
  R.add_function r fundecl

let add_functions
    env ~params_and_body r { SCCSC.code; set_of_closures = _; }  =
  let aux code_id SC.Code.({ params_and_body = p; newer_version_of = _; }) r =
    match (p : _ SC.Code.or_deleted) with
    | Deleted -> r
    | Present p -> add_function env r ~params_and_body code_id p
  in
  Code_id.Map.fold aux code r

let preallocate_set_of_closures
    (r, updates, env)
    { BSCSC.code_ids = _; closure_symbols; }
    { SCCSC.code = _; set_of_closures; } =
  let env, data, updates =
    static_set_of_closures env closure_symbols set_of_closures updates
  in
  let r = R.set_data r data in
  R.archive_data r, updates, env

let static_const0 env r ~params_and_body (bound_symbols : Bound_symbols.t)
      (static_const : Static_const.t) =
  match bound_symbols, static_const with
  | Singleton s, Block (tag, _mut, fields) ->
      let name = symbol s in
      let tag = Tag.Scannable.to_int tag in
      let block_name = name, Cmmgen_state.Global in
      let header = C.block_header tag (List.length fields) in
      let env, static_fields =
        List.fold_right
          (fun v (env, static_fields) ->
             let env, static_field = static_value env v in
             env, static_field :: static_fields)
          fields (env, [])
      in
      let block = C.emit_block block_name header static_fields in
      let updates = static_block_updates (C.symbol name) env None 0 fields in
      env, R.set_data r block, updates
  | Sets_of_closures binders (*{ code_ids = _; closure_symbols; }*),
    Sets_of_closures definitions (*{ code; set_of_closures; }*) ->
      (* We cannot both build the environment and compile the functions in
         one traversal, as the bodies may contain direct calls to the code ids
         being defined *)
      let updated_env =
        List.fold_left update_env_for_set_of_closure env definitions
      in
      let r =
        List.fold_left (add_functions updated_env ~params_and_body) r definitions
      in
      let r, updates, env =
        List.fold_left2 preallocate_set_of_closures
          (r, None, updated_env) binders definitions
      in
      env, r, updates
  | Singleton s, Boxed_float v ->
      let default = Numbers.Float_by_bit_pattern.zero in
      let transl = Numbers.Float_by_bit_pattern.to_float in
      let r, updates =
        static_boxed_number
          Cmm.Double_u env s default C.emit_float_constant transl v r
      in
      env, r, updates
  | Singleton s, Boxed_int32 v ->
      let r, updates =
        static_boxed_number
          Cmm.Word_int env s 0l C.emit_int32_constant Fun.id v r
      in
      env, r, updates
  | Singleton s, Boxed_int64 v ->
      let r, updates =
        static_boxed_number
          Cmm.Word_int env s 0L C.emit_int64_constant Fun.id v r
      in
      env, r, updates
  | Singleton s, Boxed_nativeint v ->
      let default = Targetint.zero in
      let transl = nativeint_of_targetint in
      let r, updates =
        static_boxed_number
          Cmm.Word_int env s default C.emit_nativeint_constant transl v r
      in
      env, r, updates
  | Singleton s,
    (Immutable_float_block fields | Immutable_float_array fields) ->
      let name = symbol s in
      let aux =
        Or_variable.value_map ~default:0.
          ~f:Numbers.Float_by_bit_pattern.to_float
      in
      let static_fields = List.map aux fields in
      let float_array =
        C.emit_float_array_constant (name, Cmmgen_state.Global) static_fields
      in
      let e = static_float_array_updates (C.symbol name) env None 0 fields in
      env, R.update_data r float_array, e
  | Singleton s, Mutable_string { initial_value = str; }
  | Singleton s, Immutable_string str ->
      let name = symbol s in
      let data = C.emit_string_constant (name, Cmmgen_state.Global) str in
      env, R.update_data r data, None
  | Singleton _, Sets_of_closures _ ->
      Misc.fatal_errorf "[Code_and_set_of_closures] cannot be bound by a \
          [Singleton] binding:@ %a"
        SC.print static_const
  | Sets_of_closures _,
    (Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Immutable_float_block _
      | Immutable_float_array _ | Mutable_string _ | Immutable_string _) ->
      Misc.fatal_errorf "Only [Code_and_set_of_closures] can be bound by a \
          [Code_and_set_of_closures] binding:@ %a"
        SC.print static_const

let static_const
    env r ~params_and_body
    (bound_symbols : Bound_symbols.t)
    (static_const : Static_const.t) =
  try
    (* Gc roots: statically allocated blocks themselves do not need to be scanned,
       however if statically allocated blocks contain dynamically allocated
       contents, then that block has to be registered as Gc roots for the Gc to
       correctly patch it if/when it moves some of the dynamically allocated
       blocks. As a safe over-approximation, we thus register as gc_roots all
       symbols who have an associated computation (and thus are not
       fully_static). *)
    let roots =
      if Static_const.is_fully_static static_const then []
      else Symbol.Set.elements (Bound_symbols.being_defined bound_symbols)
    in
    let r = R.add_gc_roots r roots in
    let env, r, update_opt =
      static_const0 env r ~params_and_body bound_symbols static_const
    in
    (* [R.archive_data] helps keep definitions of separate symbols in different
       [data_item] lists and this increases readability of the generated Cmm. *)
    env, R.archive_data r, update_opt
  with Misc.Fatal_error as e ->
    if !Clflags.flambda_context_on_error then begin
      (* Create a new let_symbol with a dummy body to better
         print the ound symbols and static const. *)
      let dummy_body = Expr.create_invalid () in
      let tmp_let_symbol =
        Let_symbol.create Syntactic bound_symbols static_const dummy_body
      in
      Format.eprintf
        "\n@[<v 0>%sContext is:%s translating let_symbol to Cmm:@ %a@."
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        Let_symbol.print tmp_let_symbol
    end;
    raise e

