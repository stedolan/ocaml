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

open! Flambda.Import

module I = Simplify_envs_intf
module KP = Kinded_parameter
module T = Flambda_type
module TE = Flambda_type.Typing_env

type resolver = Compilation_unit.t -> Flambda_type.Typing_env.t option
type get_imported_names = unit -> Name.Set.t
type get_imported_code = unit -> Exported_code.t

module rec Downwards_env : sig
  include I.Downwards_env
    with type lifted_constant := Lifted_constant.t
    with type lifted_constant_state := Lifted_constant_state.t
end = struct
  type t = {
    backend : (module Flambda_backend_intf.S);
    round : int;
    typing_env : TE.t;
    get_imported_code : (unit -> Exported_code.t);
    inlined_debuginfo : Debuginfo.t;
    can_inline : bool;
    inlining_depth_increment : int;
    float_const_prop : bool;
    code : Function_params_and_body.t Code_id.Map.t;
    at_unit_toplevel : bool;
    unit_toplevel_exn_continuation : Continuation.t;
    symbols_currently_being_defined : Symbol.Set.t;
  }

  let print ppf { backend = _; round; typing_env; get_imported_code = _;
                  inlined_debuginfo; can_inline;
                  inlining_depth_increment; float_const_prop;
                  code; at_unit_toplevel; unit_toplevel_exn_continuation;
                  symbols_currently_being_defined;
                } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(round@ %d)@]@ \
        @[<hov 1>(typing_env@ %a)@]@ \
        @[<hov 1>(inlined_debuginfo@ %a)@]@ \
        @[<hov 1>(can_inline@ %b)@]@ \
        @[<hov 1>(inlining_depth_increment@ %d)@]@ \
        @[<hov 1>(float_const_prop@ %b)@]@ \
        @[<hov 1>(at_unit_toplevel@ %b)@]@ \
        @[<hov 1>(unit_toplevel_exn_continuation@ %a)@]@ \
        @[<hov 1>(symbols_currently_being_defined@ %a)@]@ \
        @[<hov 1>(code@ %a)@]\
        )@]"
      round
      TE.print typing_env
      Debuginfo.print inlined_debuginfo
      can_inline
      inlining_depth_increment
      float_const_prop
      at_unit_toplevel
      Continuation.print unit_toplevel_exn_continuation
      Symbol.Set.print symbols_currently_being_defined
      (Code_id.Map.print Function_params_and_body.print) code

  let invariant _t = ()

  let create ~round ~backend ~(resolver : resolver)
        ~(get_imported_names : get_imported_names)
        ~(get_imported_code : get_imported_code)
        ~float_const_prop ~unit_toplevel_exn_continuation =
    { backend;
      round;
      typing_env = TE.create ~resolver ~get_imported_names;
      get_imported_code;
      inlined_debuginfo = Debuginfo.none;
      can_inline = true;
      inlining_depth_increment = 0;
      float_const_prop;
      code = Code_id.Map.empty;
      at_unit_toplevel = true;
      unit_toplevel_exn_continuation;
      symbols_currently_being_defined = Symbol.Set.empty;
    }

  let resolver t = TE.resolver t.typing_env
  let backend t = t.backend
  let typing_env t = t.typing_env
  let round t = t.round
  let get_continuation_scope_level t = TE.current_scope t.typing_env
  let can_inline t = t.can_inline
  let float_const_prop t = t.float_const_prop

  let unit_toplevel_exn_continuation t = t.unit_toplevel_exn_continuation

  let at_unit_toplevel t = t.at_unit_toplevel

  let set_not_at_unit_toplevel t =
    { t with at_unit_toplevel = false; }

  let get_inlining_depth_increment t = t.inlining_depth_increment

  let set_inlining_depth_increment t inlining_depth_increment =
    { t with inlining_depth_increment; }

  (* CR mshinwell: remove "_level" *)
  let increment_continuation_scope_level t =
    { t with
      typing_env = TE.increment_scope t.typing_env;
    }

  let increment_continuation_scope_level_twice t =
    increment_continuation_scope_level
      (increment_continuation_scope_level t)

  let now_defining_symbol t symbol =
    if Symbol.Set.mem symbol t.symbols_currently_being_defined then begin
      Misc.fatal_errorf "Already defining symbol %a:@ %a"
        Symbol.print symbol
        print t
    end;
    let symbols_currently_being_defined =
      Symbol.Set.add symbol t.symbols_currently_being_defined
    in
    { t with
      symbols_currently_being_defined;
    }

  let no_longer_defining_symbol t symbol =
    if not (Symbol.Set.mem symbol t.symbols_currently_being_defined) then begin
      Misc.fatal_errorf "Not currently defining symbol %a:@ %a"
        Symbol.print symbol
        print t
    end;
    let symbols_currently_being_defined =
      Symbol.Set.remove symbol t.symbols_currently_being_defined
    in
    { t with
      symbols_currently_being_defined;
    }

  let symbol_is_currently_being_defined t symbol =
    Symbol.Set.mem symbol t.symbols_currently_being_defined

  let symbols_currently_being_defined t =
    t.symbols_currently_being_defined

  let enter_closure { backend; round; typing_env; get_imported_code;
                      inlined_debuginfo = _; can_inline;
                      inlining_depth_increment;
                      float_const_prop; code; at_unit_toplevel = _;
                      unit_toplevel_exn_continuation;
                      symbols_currently_being_defined;
                    } =
    { backend;
      round;
      typing_env = TE.closure_env typing_env;
      get_imported_code;
      inlined_debuginfo = Debuginfo.none;
      can_inline;
      inlining_depth_increment;
      float_const_prop;
      code;
      at_unit_toplevel = false;
      unit_toplevel_exn_continuation;
      symbols_currently_being_defined;
    }

  let define_variable t var kind =
    let typing_env =
      let var = Name_in_binding_pos.var var in
      TE.add_definition t.typing_env var kind
    in
    { t with typing_env; }

  let add_name t name ty =
    let typing_env =
      TE.add_equation
        (TE.add_definition t.typing_env name (T.kind ty))
        (Name_in_binding_pos.name name) ty
    in
    { t with typing_env; }

  let add_variable t var ty =
    let typing_env =
      let var' = Name_in_binding_pos.var var in
      TE.add_equation
        (TE.add_definition t.typing_env var' (T.kind ty))
        (Name.var (Var_in_binding_pos.var var)) ty
    in
    { t with typing_env; }

  let add_equation_on_variable t var ty =
    let typing_env = TE.add_equation t.typing_env (Name.var var) ty in
    { t with typing_env; }

  let mem_name t name = TE.mem t.typing_env name

  let find_name t name =
    match TE.find t.typing_env name None with
    | exception Not_found ->
      Misc.fatal_errorf "Unbound name %a in environment:@ %a"
        Name.print name
        print t
    | ty -> ty

  let find_variable t var = find_name t (Name.var var)

  let mem_variable t var = TE.mem t.typing_env (Name.var var)

  let define_symbol t sym kind =
    let typing_env =
      let sym =
        Name_in_binding_pos.create (Name.symbol sym)
          Name_mode.normal
      in
      TE.add_definition t.typing_env sym kind
    in
    { t with typing_env; }

  let define_symbol_if_undefined t sym kind =
    if TE.mem t.typing_env (Name.symbol sym) then t
    else define_symbol t sym kind

  let add_symbol t sym ty =
    let typing_env =
      let sym = Name.symbol sym in
      let sym' = Name_in_binding_pos.create sym Name_mode.normal in
      TE.add_equation
        (TE.add_definition t.typing_env sym' (T.kind ty))
        sym ty
    in
    { t with typing_env; }

  let add_equation_on_symbol t sym ty =
    let typing_env =
      let sym = Name.symbol sym in
      TE.add_equation t.typing_env sym ty
    in
    { t with typing_env; }

  let mem_symbol t sym = mem_name t (Name.symbol sym)

  let find_symbol t sym = find_name t (Name.symbol sym)

  let define_name t name kind =
    let typing_env =
      TE.add_definition t.typing_env name kind
    in
    { t with typing_env; }

  let define_name_if_undefined t name kind =
    if TE.mem t.typing_env (Name_in_binding_pos.to_name name) then t
    else
      let typing_env =
        TE.add_definition t.typing_env name kind
      in
      { t with typing_env; }

  let add_equation_on_name t name ty =
    let typing_env = TE.add_equation t.typing_env name ty in
    { t with typing_env; }

(*
  let add_symbol_if_not_defined t sym ty =
    let name = Name.symbol sym in
    if TE.mem t.typing_env name then t
    else add_symbol t sym ty
*)

  let define_parameters t ~params =
    List.fold_left (fun t param ->
        let var =
          Var_in_binding_pos.create (KP.var param) Name_mode.normal
        in
        define_variable t var (KP.kind param))
      t
      params

  let define_parameters_as_bottom t ~params =
    List.fold_left (fun t param ->
        let var =
          Var_in_binding_pos.create (KP.var param) Name_mode.normal
        in
        let kind = KP.kind param in
        let t = define_variable t var kind in
        add_equation_on_variable t (KP.var param) (T.bottom kind))
      t
      params

  let add_parameters t params ~param_types =
    if List.compare_lengths params param_types <> 0 then begin
      Misc.fatal_errorf "Mismatch between number of [params] and \
          [param_types]:@ (%a)@ and@ %a"
        Kinded_parameter.List.print params
        (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print) param_types
    end;
    List.fold_left2 (fun t param param_type ->
        let var =
          Var_in_binding_pos.create (KP.var param) Name_mode.normal
        in
        add_variable t var param_type)
      t
      params param_types

  let add_parameters_with_unknown_types t params =
    let param_types =
      List.map (fun param -> T.unknown (KP.kind param)) params
    in
    add_parameters t params ~param_types

  let extend_typing_environment t env_extension =
    let typing_env = TE.add_env_extension t.typing_env env_extension in
    { t with
      typing_env;
    }

  let with_typing_env t typing_env =
    { t with
      typing_env;
    }

  let map_typing_env t ~f = with_typing_env t (f t.typing_env)

  let check_variable_is_bound t var =
    if not (TE.mem t.typing_env (Name.var var)) then begin
      Misc.fatal_errorf "Unbound variable %a in environment:@ %a"
        Variable.print var
        print t
    end

  let check_symbol_is_bound t sym =
    if not (TE.mem t.typing_env (Name.symbol sym)) then begin
      Misc.fatal_errorf "Unbound symbol %a in environment:@ %a"
        Symbol.print sym
        print t
    end

  let check_name_is_bound t name =
    if not (TE.mem t.typing_env name) then begin
      Misc.fatal_errorf "Unbound name %a in environment:@ %a"
        Name.print name
        print t
    end

  let check_simple_is_bound t (simple : Simple.t) =
    Simple.pattern_match simple
      ~name:(fun name -> check_name_is_bound t name)
      ~const:(fun _ -> ())

  let mem_code t code_id =
    Code_id.Map.mem code_id t.code
      || Exported_code.mem code_id (t.get_imported_code ())

  let check_code_id_is_bound t code_id =
    if (not (Code_id.Map.mem code_id t.code))
      && (not (Exported_code.mem code_id (t.get_imported_code ())))
    then begin
      Misc.fatal_errorf "Unbound code ID %a in environment:@ %a"
        Code_id.print code_id
        print t
    end

  let define_code t ?newer_version_of ~code_id ~params_and_body:code =
    if not (Code_id.in_compilation_unit code_id
      (Compilation_unit.get_current_exn ()))
    then begin
      Misc.fatal_errorf "Cannot define code ID %a as it is from another unit:\
          @ %a"
        Code_id.print code_id
        Function_params_and_body.print code
    end;
    if Code_id.Map.mem code_id t.code then begin
      Misc.fatal_errorf "Code ID %a is already defined, cannot redefine to@ %a"
        Code_id.print code_id
        Function_params_and_body.print code
    end;
    let typing_env =
      match newer_version_of with
      | None -> t.typing_env
      | Some older ->
        TE.add_to_code_age_relation t.typing_env ~newer:code_id ~older
    in
    { t with
      typing_env;
      code = Code_id.Map.add code_id code t.code;
    }

  let find_code t id =
    match Code_id.Map.find id t.code with
    | exception Not_found ->
      Exported_code.find_code (t.get_imported_code ()) id
    | code -> code

  let with_code ~from t =
    { t with
      code = from.code;
    }

  let add_lifted_constants t lifted =
    let module LC = Lifted_constant in
    let module LCS = Lifted_constant_state in
    let t =
      LCS.fold lifted ~init:t ~f:(fun t lifted_constant ->
        let types_of_symbols = LC.types_of_symbols lifted_constant in
        Symbol.Map.fold (fun sym (_denv, typ) t ->
            define_symbol t sym (T.kind typ))
          types_of_symbols
          t)
    in
    let typing_env =
      LCS.fold lifted ~init:t.typing_env ~f:(fun typing_env lifted_constant ->
        let types_of_symbols = LC.types_of_symbols lifted_constant in
        Symbol.Map.fold (fun sym (denv_at_definition, typ) typing_env ->
            let sym = Name.symbol sym in
            let env_extension =
              (* CR mshinwell: Sometimes we might already have the types
                 "made suitable" in the [closure_env] field of the typing
                 environment, perhaps?  For example when lifted constants'
                 types are coming out of a closure into the enclosing
                 scope. *)
              T.make_suitable_for_environment typ
                denv_at_definition.typing_env
                ~suitable_for:typing_env
                ~bind_to:sym
            in
            TE.add_env_extension typing_env env_extension)
          types_of_symbols
          typing_env)
    in
    LCS.fold lifted ~init:(with_typing_env t typing_env)
      ~f:(fun denv lifted_constant ->
        let defining_expr = LC.defining_expr lifted_constant in
        Code_id.Lmap.fold
          (fun code_id
               ({ params_and_body; newer_version_of; } : Static_const.Code.t)
               denv ->
            match params_and_body with
            | Present params_and_body ->
              define_code denv ?newer_version_of ~code_id ~params_and_body
            | Deleted -> denv)
          (Static_const.get_pieces_of_code defining_expr)
          denv)

  let add_lifted_constant t const =
    add_lifted_constants t (Lifted_constant_state.singleton const)

  let set_inlined_debuginfo t dbg =
    { t with inlined_debuginfo = dbg; }

  let add_inlined_debuginfo' t dbg =
    Debuginfo.concat t.inlined_debuginfo dbg

  let add_inlined_debuginfo t dbg =
    if List.length dbg > 100 || List.length t.inlined_debuginfo > 100 then
      Misc.fatal_errorf "STOP@ %a\n%!" print t;
    { t with
      inlined_debuginfo = add_inlined_debuginfo' t dbg
    }

  let disable_function_inlining t =
    { t with
      can_inline = false;
    }

end and Upwards_env : sig
  include I.Upwards_env
    with type downwards_env := Downwards_env.t
end = struct
  type t = {
    continuations : (Scope.t * Continuation_in_env.t) Continuation.Map.t;
    exn_continuations : Scope.t Exn_continuation.Map.t;
    continuation_aliases : Continuation.t Continuation.Map.t;
    apply_cont_rewrites : Apply_cont_rewrite.t Continuation.Map.t;
  }

  let invariant _t = ()

  let empty =
    { continuations = Continuation.Map.empty;
      exn_continuations = Exn_continuation.Map.empty;
      continuation_aliases = Continuation.Map.empty;
      apply_cont_rewrites = Continuation.Map.empty;
    }

  let print_scope_level_and_continuation_in_env ppf (scope_level, cont_in_env) =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(scope_level@ %a)@]@ \
        @[<hov 1>(cont_in_env@ %a)@]\
        )@]"
      Scope.print scope_level
      Continuation_in_env.print cont_in_env

  let print ppf { continuations; exn_continuations; continuation_aliases;
                  apply_cont_rewrites;
                } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(continuations@ %a)@]@ \
        @[<hov 1>(exn_continuations@ %a)@]@ \
        @[<hov 1>(continuation_aliases@ %a)@]@ \
        @[<hov 1>(apply_cont_rewrites@ %a)@]\
        )@]"
      (Continuation.Map.print print_scope_level_and_continuation_in_env)
      continuations
      (Exn_continuation.Map.print Scope.print) exn_continuations
      (Continuation.Map.print Continuation.print) continuation_aliases
      (Continuation.Map.print Apply_cont_rewrite.print)
      apply_cont_rewrites

  let find_continuation t cont =
    match Continuation.Map.find cont t.continuations with
    | exception Not_found ->
      Misc.fatal_errorf "Unbound continuation %a in upwards environment:@ %a"
        Continuation.print cont
        print t
    | (_scope_level, cont_in_env) -> cont_in_env

  let mem_continuation t cont =
    Continuation.Map.mem cont t.continuations

  let resolve_continuation_aliases t cont =
    match Continuation.Map.find cont t.continuation_aliases with
    | exception Not_found -> cont
    | alias_for -> alias_for

  let resolve_exn_continuation_aliases t exn_cont =
    let cont = Exn_continuation.exn_handler exn_cont in
    match Continuation.Map.find cont t.continuation_aliases with
    | exception Not_found -> exn_cont
    | alias_for -> Exn_continuation.with_exn_handler exn_cont alias_for

  let continuation_arity t cont =
    match find_continuation t cont with
    | Unknown { arity; handler = _; }
    | Unreachable { arity; }
    | Inline { arity; _ } -> arity

  let add_continuation0 t cont scope cont_in_env =
    let continuations =
      Continuation.Map.add cont (scope, cont_in_env) t.continuations
    in
    { t with
      continuations;
    }

  let add_continuation t cont scope arity =
    add_continuation0 t cont scope (Unknown { arity; handler = None; })

  let add_continuation_with_handler t cont scope arity handler =
    add_continuation0 t cont scope (Unknown { arity; handler = Some handler; })

  let add_unreachable_continuation t cont scope arity =
    add_continuation0 t cont scope (Unreachable { arity; })

  let add_continuation_alias t cont arity ~alias_for =
    let alias_for = resolve_continuation_aliases t alias_for in
    let alias_for_arity = continuation_arity t alias_for in
    if not (Flambda_arity.equal arity alias_for_arity) then begin
      Misc.fatal_errorf "%a (arity %a) cannot be an alias for %a (arity %a) \
          since the two continuations differ in arity"
        Continuation.print cont
        Flambda_arity.print arity
        Continuation.print alias_for
        Flambda_arity.print alias_for_arity
    end;
    if Continuation.Map.mem cont t.continuation_aliases then begin
      Misc.fatal_errorf "Cannot add continuation alias %a (as alias for %a); \
          the continuation is already deemed to be an alias"
        Continuation.print cont
        Continuation.print alias_for
    end;
(* CR mshinwell: This should check that they are either both exn handlers
   or both non-exn handlers
    if Continuation.is_exn cont || Continuation.is_exn alias_for then begin
      Misc.fatal_errorf "Cannot alias exception handlers: %a (exn handler? %b) \
          as alias for %a (exn handler? %b)"
        Continuation.print cont
        (Continuation.is_exn cont)
        Continuation.print alias_for
        (Continuation.is_exn alias_for)
    end;
*)
    let alias_for = resolve_continuation_aliases t alias_for in
    let continuation_aliases =
      Continuation.Map.add cont alias_for t.continuation_aliases
    in
    { t with
      continuation_aliases;
    }

  let add_continuation_to_inline t cont scope arity handler =
    add_continuation0 t cont scope (Inline { arity; handler; })

  let add_exn_continuation t exn_cont scope =
    (* CR mshinwell: Think more about keeping these in both maps *)
    let continuations =
      let cont = Exn_continuation.exn_handler exn_cont in
      let cont_in_env : Continuation_in_env.t =
        Unknown { arity = Exn_continuation.arity exn_cont; handler = None; }
      in
      Continuation.Map.add cont (scope, cont_in_env) t.continuations
    in
    let exn_continuations =
      Exn_continuation.Map.add exn_cont scope t.exn_continuations
    in
    { t with
      continuations;
      exn_continuations;
    }

  let check_continuation_is_bound t cont =
    if not (Continuation.Map.mem cont t.continuations) then begin
      Misc.fatal_errorf "Unbound continuation %a in environment:@ %a"
        Continuation.print cont
        print t
    end

  let check_exn_continuation_is_bound t exn_cont =
    if not (Exn_continuation.Map.mem exn_cont t.exn_continuations) then begin
      Misc.fatal_errorf "Unbound exception continuation %a in environment:@ %a"
        Exn_continuation.print exn_cont
        print t
    end

  let add_apply_cont_rewrite t cont rewrite =
    if Continuation.Map.mem cont t.apply_cont_rewrites then begin
      Misc.fatal_errorf "Cannot redefine [Apply_cont_rewrite] for %a"
        Continuation.print cont
    end;
    let apply_cont_rewrites =
      Continuation.Map.add cont rewrite t.apply_cont_rewrites
    in
    { t with
      apply_cont_rewrites;
    }

  let find_apply_cont_rewrite t cont =
    match Continuation.Map.find cont t.apply_cont_rewrites with
    | exception Not_found -> None
    | rewrite -> Some rewrite
end and Lifted_constant : sig
  include I.Lifted_constant
    with type downwards_env := Downwards_env.t
end = struct
  type for_one_set_of_closures = {
    code_ids : Code_id.Set.t;
    denv : Downwards_env.t option;
    closure_symbols_with_types : (Symbol.t * Flambda_type.t) Closure_id.Lmap.t;
  }

  type descr =
    | Singleton of {
        denv : Downwards_env.t;
        symbol : Symbol.t;
        ty : Flambda_type.t;
        defining_expr : Flambda.Static_const.t;
      }
    | Sets_of_closures of {
        sets : for_one_set_of_closures list;
        defining_expr : Flambda.Static_const.t;
      }

  type t = descr

  let descr t = t

  let bound_symbols t : Bound_symbols.t =
    match t with
    | Singleton { symbol; _ } -> Singleton symbol
    | Sets_of_closures { sets; _ } ->
      let sets =
        ListLabels.map sets
          ~f:(fun for_one_set : Bound_symbols.Code_and_set_of_closures.t ->
            { code_ids = for_one_set.code_ids;
              closure_symbols =
                Closure_id.Lmap.map fst for_one_set.closure_symbols_with_types;
            })
      in
      Sets_of_closures sets

  let defining_expr t =
    match t with
    | Singleton { defining_expr; _ }
    | Sets_of_closures { defining_expr; _ } -> defining_expr

  (* CR-soon mshinwell: Update this to print everything *)
  let print ppf t =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(bound_symbols@ %a)@]@ \
        @[<hov 1>(static_const@ %a)@]\
        )@]"
      Bound_symbols.print (bound_symbols t)
      Static_const.print (defining_expr t)

  let types_of_symbols t =
    match t with
    | Singleton { symbol; denv; ty; _ } ->
      Symbol.Map.singleton symbol (denv, ty)
    | Sets_of_closures { sets; _ } ->
      ListLabels.fold_left sets ~init:Symbol.Map.empty
        ~f:(fun types_of_symbols for_one_set ->
          let denv = for_one_set.denv in
          Closure_id.Lmap.fold (fun _closure_id (symbol, ty) types_of_symbols ->
              match denv with
              | Some denv -> Symbol.Map.add symbol (denv, ty) types_of_symbols
              | None ->
                Misc.fatal_errorf "Missing denv:@ %a" print t)
            for_one_set.closure_symbols_with_types
            types_of_symbols)

  let create_singleton symbol defining_expr denv ty =
    (* CR mshinwell: check that [defining_expr] is not a set of closures *)
    Singleton {
      symbol;
      denv;
      ty;
      defining_expr;
    }

  let create_multiple_sets_of_closures sets defining_expr =
    (* CR mshinwell: Check that [sets] matches [defining_expr] and
       that [defining_expr] is a set of closures *)
    Sets_of_closures {
      sets;
      defining_expr;
    }

  let create_set_of_closures code_ids denv ~closure_symbols_with_types
        defining_expr =
    create_multiple_sets_of_closures
      [{ code_ids; denv = Some denv; closure_symbols_with_types; }]
      defining_expr

  let create_pieces_of_code ?newer_versions_of code =
    (* CR mshinwell: Avoid going via [pieces_of_code]? *)
    let bound_symbols, defining_expr =
      Flambda.pieces_of_code ?newer_versions_of code
    in
    match bound_symbols with
    | Sets_of_closures [{ code_ids; closure_symbols; }] ->
      assert (Closure_id.Lmap.is_empty closure_symbols);
      create_multiple_sets_of_closures
        [{ code_ids;
           denv = None;
           closure_symbols_with_types = Closure_id.Lmap.empty;
         }]
        defining_expr
    | Sets_of_closures _
    | Singleton _ -> Misc.fatal_error "Expected singleton [Sets_of_closures]"

  let create_piece_of_code ?newer_version_of code_id params_and_body =
    let newer_versions_of =
      match newer_version_of with
      | None -> None
      | Some older -> Some (Code_id.Map.singleton code_id older)
    in
    create_pieces_of_code ?newer_versions_of
      (Code_id.Lmap.singleton code_id params_and_body)

  let create_deleted_piece_of_code ?newer_versions_of code_id =
    (* CR mshinwell: Avoid going via [deleted_pieces_of_code]? *)
    let bound_symbols, defining_expr =
      Flambda.deleted_pieces_of_code ?newer_versions_of
        (Code_id.Set.singleton code_id)
    in
    match bound_symbols with
    | Sets_of_closures [{ code_ids; closure_symbols; }] ->
      assert (Closure_id.Lmap.is_empty closure_symbols);
      create_multiple_sets_of_closures
        [{ code_ids;
           denv = None;
           closure_symbols_with_types = Closure_id.Lmap.empty;
         }]
        defining_expr
    | Sets_of_closures _
    | Singleton _ -> Misc.fatal_error "Expected singleton [Sets_of_closures]"
end and Lifted_constant_state : sig
  include I.Lifted_constant_state
    with type lifted_constant := Lifted_constant.t
end = struct
  type t =
    | Empty
    | Leaf of Lifted_constant.t
    | Union of t * t
    | Union_leaf of t * Lifted_constant.t

  let to_list t =
    let rec to_list t acc =
      match t with
      | Empty -> acc
      | Leaf const -> const :: acc
      | Union (t1, t2) -> to_list t1 (to_list t2 acc)
      | Union_leaf (t, const) -> to_list t (const :: acc)
    in
    to_list t []

  let print ppf t =
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Lifted_constant.print)
      (to_list t)

  let empty = Empty

  let union t1 t2 =
    match t1, t2 with
    | Empty, _ -> t2
    | _, Empty -> t1
    | Leaf const, (Union _ | Union_leaf _) -> Union_leaf (t2, const)
    | (Union _ | Union_leaf _), Leaf const -> Union_leaf (t1, const)
    | (Leaf _ | Union _ | Union_leaf _), _ -> Union (t1, t2)

  let is_empty t =
    match t with
    | Empty -> true
    | Leaf _ | Union _ | Union_leaf _ -> false

  let singleton const = Leaf const

  let add t const = Union (t, Leaf const)

  let rec fold t ~init ~f =
    match t with
    | Empty -> init
    | Leaf const -> f init const
    | Union (t1, t2) -> fold t2 ~init:(fold t1 ~init ~f) ~f
    | Union_leaf (t, const) -> f (fold t ~init ~f) const
end
