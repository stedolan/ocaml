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

module K = Flambda_kind

(* CR mshinwell: Add signatures to these submodules. *)
module Cached : sig
  type t

  val print_name_modes
     : restrict_to:Name.Set.t
    -> Format.formatter
    -> t
    -> unit

  val empty : t

  val names_to_types
     : t
    -> (Type_grammar.t * Binding_time.t * Name_mode.t) Name.Map.t

  val aliases : t -> Aliases.t

  val cse : t -> Simple.t Flambda_primitive.Eligible_for_cse.Map.t

  val add_or_replace_binding
     : t
    -> Name.t
    -> Type_grammar.t
    -> Binding_time.t
    -> Name_mode.t
    -> new_aliases:Aliases.t
    -> t

  val replace_variable_binding
     : t
    -> Variable.t
    -> Type_grammar.t
    -> new_aliases:Aliases.t
    -> t

  val with_cse : t -> cse:Simple.t Flambda_primitive.Eligible_for_cse.Map.t -> t

  val clean_for_export : t -> module_symbol:Symbol.t -> t

  val import : Ids_for_export.Import_map.t -> t -> t

  val merge : t -> t -> t
end = struct
  type t = {
    names_to_types :
      (Type_grammar.t * Binding_time.t * Name_mode.t) Name.Map.t;
    aliases : Aliases.t;
    cse : Simple.t Flambda_primitive.Eligible_for_cse.Map.t;
  }

  let print_kind_and_mode ppf (ty, _, mode) =
    let kind = Type_grammar.kind ty in
    Format.fprintf ppf ":: %a %a"
      Flambda_kind.print kind
      Name_mode.print mode

  let print_name_modes ~restrict_to ppf t =
    Name.Map.print print_kind_and_mode ppf
      (Name.Map.filter (fun name _ -> Name.Set.mem name restrict_to)
        t.names_to_types)

(*
  let _print_with_cache ~cache ppf { names_to_types; aliases; } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(names_to_types@ %a)@]@ \
        @[<hov 1>(aliases@ %a)@]\
        )@]"
      (Name.Map.print (Type_grammar.print_with_cache ~cache)) names_to_types
      Aliases.print aliases

  let _invariant t =
    let canonical_names = Aliases.canonical_names t.aliases in
    Name.Set.iter (fun name ->
        match Name.Map.find name t.names_to_types with
        | exception Not_found ->
          Misc.fatal_errorf "Canonical name %a not in [names_to_types]"
            Name.print name
        | ty ->
          match Type_grammar.get_alias ty with
          | None -> ()
          | Some alias_of ->
            Misc.fatal_errorf "Canonical name %a has an alias type: =%a"
              Name.print name
              Simple.print alias_of)
      canonical_names
*)

  (* CR mshinwell: add [invariant] function *)

  let empty =
    { names_to_types = Name.Map.empty;
      aliases = Aliases.empty;
      cse = Flambda_primitive.Eligible_for_cse.Map.empty;
    }

  let names_to_types t = t.names_to_types
  let aliases t = t.aliases
  let cse t = t.cse

  (* CR mshinwell: At least before the following two functions were split
     (used to be add-or-replace), the [names_to_types] map addition was a
     major source of allocation. *)

  let add_or_replace_binding t (name : Name.t) ty binding_time name_mode ~new_aliases =
    let names_to_types =
      Name.Map.add name (ty, binding_time, name_mode) t.names_to_types
    in
    { names_to_types;
      aliases = new_aliases;
      cse = t.cse;
    }

  let replace_variable_binding t var ty ~new_aliases =
    let names_to_types =
      Name.Map.replace (Name.var var)
        (function (_old_ty, binding_time, name_mode) ->
          ty, binding_time, name_mode)
        t.names_to_types
    in
    { names_to_types;
      aliases = new_aliases;
      cse = t.cse;
    }

  let with_cse t ~cse = { t with cse; }

  let clean_for_export t =
    (* Two things happen:
       - All variables are existentialized (mode is switched to in_types)
       - Names coming from other compilation units are removed
    *)
    let names_to_types =
      Name.Map.mapi (fun name ((ty, binding_time, _mode) as info) ->
          Name.pattern_match name
          ~var:(fun _ -> ty, binding_time, Name_mode.in_types)
          ~symbol:(fun _ -> info))
        t.names_to_types
    in
    let current_compilation_unit = Compilation_unit.get_current_exn () in
    let names_to_types =
      Name.Map.filter (fun name _info ->
          Compilation_unit.equal (Name.compilation_unit name)
            current_compilation_unit)
        names_to_types
    in
    { t with
      names_to_types;
    }

  let clean_for_export t ~module_symbol:_ =
    clean_for_export t

  let import import_map { names_to_types; aliases; cse; } =
    let module Import = Ids_for_export.Import_map in
    let names_to_types =
      Name.Map.fold (fun name (ty, binding_time, mode) acc ->
          Name.Map.add (Import.name import_map name)
            (Type_grammar.import import_map ty, binding_time, mode)
            acc)
        names_to_types
        Name.Map.empty
    in
    let aliases = Aliases.import import_map aliases in
    let cse =
      Flambda_primitive.Eligible_for_cse.Map.fold (fun prim rhs acc ->
          let (), prim =
            Flambda_primitive.Eligible_for_cse.fold_args prim
              ~init:()
              ~f:(fun () simple -> (), Import.simple import_map simple)
          in
          Flambda_primitive.Eligible_for_cse.Map.add prim
            (Import.simple import_map rhs)
            acc)
        cse
        Flambda_primitive.Eligible_for_cse.Map.empty
    in
    { names_to_types; aliases; cse }

  let merge t1 t2 =
    let names_to_types =
      Name.Map.disjoint_union
        t1.names_to_types
        t2.names_to_types
    in
    let aliases =
      Aliases.merge t1.aliases t2.aliases
    in
    let cse =
      Flambda_primitive.Eligible_for_cse.Map.union
        (fun prim simple1 simple2 ->
           let cannot_merge _name =
             if Name_occurrences.is_empty
                  (Flambda_primitive.Eligible_for_cse.free_names prim)
             then None
             else
               Misc.fatal_errorf
                 "Cannot merge CSE equation %a from different environments"
                 Flambda_primitive.Eligible_for_cse.print prim
           in
           Simple.pattern_match simple1
             ~const:(fun const1 ->
               Simple.pattern_match simple2
                 ~const:(fun const2 ->
                   if Reg_width_things.Const.equal const1 const2
                   then Some simple1
                   else
                     Misc.fatal_errorf
                       "Inconsistent values for CSE equation@ %a:@ %a@ <> %a"
                       Flambda_primitive.Eligible_for_cse.print prim
                       Simple.print simple1 Simple.print simple2)
                 ~name:cannot_merge)
             ~name:cannot_merge)
        t1.cse t2.cse
    in
    { names_to_types; aliases; cse; }
end

module One_level = struct
  type t = {
    scope : Scope.t;
    level : Typing_env_level.t;
    just_after_level : Cached.t;
  }

  let print_with_cache ~cache:_ ppf
        { scope = _; level; just_after_level; } =
    let restrict_to = Typing_env_level.defined_names level in
    if Name.Set.is_empty restrict_to then
      Format.fprintf ppf "@[<hov 0>\
          %a\
          @]"
        Typing_env_level.print level
    else
      Format.fprintf ppf "@[<hov 0>\
          @[<hov 1>(defined_vars@ %a)@]@ \
          %a\
          @]"
        (Cached.print_name_modes ~restrict_to) just_after_level
        Typing_env_level.print level

  let create scope level ~just_after_level =
    { scope;
      level;
      just_after_level;
    }

  let create_empty scope =
    { scope;
      level = Typing_env_level.empty ();
      just_after_level = Cached.empty;
    }

  let scope t = t.scope
  let level t = t.level
  let just_after_level t = t.just_after_level

  let is_empty t = Typing_env_level.is_empty t.level

(*
  let defines_name_but_no_equations t name =
    Typing_env_level.defines_name_but_no_equations t.level name
*)

  let clean_for_export t ~module_symbol =
    { t with
      just_after_level =
        Cached.clean_for_export t.just_after_level ~module_symbol;
    }
end

type t = {
  resolver : (Compilation_unit.t -> t option);
  get_imported_names : (unit -> Name.Set.t);
  defined_symbols : Symbol.Set.t;
  code_age_relation : Code_age_relation.t;
  prev_levels : One_level.t Scope.Map.t;
  current_level : One_level.t;
  next_binding_time : Binding_time.t;
  closure_env : t option;
}

module Serializable = struct
  type typing_env = t

  type t = {
    defined_symbols : Symbol.Set.t;
    code_age_relation : Code_age_relation.t;
    just_after_level : Cached.t;
  }

  let create ({ resolver = _;
                get_imported_names = _;
                defined_symbols;
                code_age_relation;
                prev_levels = _;
                current_level;
                next_binding_time = _;
                closure_env = _; } : typing_env) =
    { defined_symbols;
      code_age_relation;
      just_after_level = One_level.just_after_level current_level;
    }

  let print ppf { defined_symbols;
                  code_age_relation;
                  just_after_level;
                } =
    Format.fprintf ppf
      "@[<hov 1>(\
          @[<hov 1>(defined_symbols@ %a)@]@ \
          @[<hov 1>(code_age_relation@ %a)@]@ \
          @[<hov 1>(type_equations@ %a)@]@ \
          @[<hov 1>(aliases@ %a)@]\
          )@]"
      Symbol.Set.print defined_symbols
      Code_age_relation.print code_age_relation
      (Name.Map.print (fun ppf (ty, _bt, _mode) -> Type_grammar.print ppf ty))
      (Cached.names_to_types just_after_level)
      Aliases.print (Cached.aliases just_after_level)

  let to_typing_env { defined_symbols;
                      code_age_relation;
                      just_after_level;
                    }
        ~resolver ~get_imported_names =
    { resolver;
      get_imported_names;
      defined_symbols;
      code_age_relation;
      prev_levels = Scope.Map.empty;
      current_level =
        One_level.create Scope.initial (Typing_env_level.empty ())
          ~just_after_level;
      next_binding_time = Binding_time.earliest_var;
      closure_env = Some {
        resolver;
        get_imported_names;
        prev_levels = Scope.Map.empty;
        current_level = One_level.create_empty Scope.initial;
        next_binding_time = Binding_time.earliest_var;
        defined_symbols = Symbol.Set.empty;
        code_age_relation = Code_age_relation.empty;
        closure_env = None;
      };
    }

  let all_ids_for_export { defined_symbols;
                           code_age_relation;
                           just_after_level; } =
    let symbols = defined_symbols in
    let code_ids = Code_age_relation.all_code_ids_for_export code_age_relation in
    let ids = Ids_for_export.create ~symbols ~code_ids () in
    let ids =
      Name.Map.fold (fun name (typ, _binding_time, _name_mode) ids ->
          Ids_for_export.add_name
            (Ids_for_export.union ids (Type_grammar.all_ids_for_export typ))
            name)
        (Cached.names_to_types just_after_level)
        ids
    in
    let ids =
      Ids_for_export.union ids
        (Aliases.all_ids_for_export (Cached.aliases just_after_level))
    in
    let ids =
      Flambda_primitive.Eligible_for_cse.Map.fold (fun prim simple ids ->
          let ids =
            Ids_for_export.union ids
              (Flambda_primitive.all_ids_for_export
                 (Flambda_primitive.Eligible_for_cse.to_primitive prim))
          in
          Ids_for_export.add_simple ids simple)
        (Cached.cse just_after_level)
        ids
    in
    ids

  let import import_map { defined_symbols;
                          code_age_relation;
                          just_after_level; } =
    let defined_symbols =
      Symbol.Set.fold (fun sym symbols ->
          Symbol.Set.add (Ids_for_export.Import_map.symbol import_map sym)
            symbols)
        defined_symbols
        Symbol.Set.empty
    in
    let code_age_relation =
      Code_age_relation.import import_map code_age_relation
    in
    let just_after_level = Cached.import import_map just_after_level in
    { defined_symbols; code_age_relation; just_after_level; }

  let merge t1 t2 =
    let defined_symbols =
      Symbol.Set.union t1.defined_symbols t2.defined_symbols
    in
    let code_age_relation =
      Code_age_relation.union t1.code_age_relation t2.code_age_relation
    in
    let just_after_level =
      Cached.merge t1.just_after_level t2.just_after_level
    in
    { defined_symbols; code_age_relation; just_after_level; }
end

let is_empty t =
  One_level.is_empty t.current_level
    && Scope.Map.is_empty t.prev_levels
    && Symbol.Set.is_empty t.defined_symbols

(* CR mshinwell: Should print name occurrence kinds *)
(* CR mshinwell: Add option to print [aliases] *)
let print_with_cache ~cache ppf
      ({ resolver = _; get_imported_names = _;
         prev_levels; current_level; next_binding_time = _;
         defined_symbols; code_age_relation; closure_env = _;
       } as t) =
  if is_empty t then
    Format.pp_print_string ppf "Empty"
  else
    let levels =
      Scope.Map.add (One_level.scope current_level) current_level prev_levels
    in
    let levels =
      Scope.Map.filter (fun _ level -> not (One_level.is_empty level))
        levels
    in
    Printing_cache.with_cache cache ppf "env" t (fun ppf () ->
      Format.fprintf ppf
        "@[<hov 1>(\
            @[<hov 1>(defined_symbols@ %a)@]@ \
            @[<hov 1>(code_age_relation@ %a)@]@ \
            @[<hov 1>(levels@ %a)@]\
            )@]"
        Symbol.Set.print defined_symbols
        Code_age_relation.print code_age_relation
        (Scope.Map.print (One_level.print_with_cache ~cache)) levels)

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant0 ?force _t =
  if !Clflags.flambda_invariant_checks || Option.is_some (force : unit option)
  then begin
(* CR mshinwell: Fix things so this check passes, or delete it.
    let no_empty_prev_levels =
      Scope.Map.for_all (fun _scope level -> not (One_level.is_empty level))
        t.prev_levels
    in
    if not no_empty_prev_levels then begin
      Misc.fatal_errorf "Typing environment contains [prev_levels] that are \
          empty:@ %a"
        print t
    end;
    let current_scope = One_level.scope t.current_level in
    let max_prev_scope =
      Scope.Map.fold (fun scope _level max_prev_scope ->
          Scope.max scope max_prev_scope)
        t.prev_levels
        Scope.initial
    in
    if (not (is_empty t))
      && Scope.(<=) current_scope max_prev_scope
    then begin
      Misc.fatal_errorf "Typing environment contains a [current_level] with a \
          scope (%a) that is not strictly greater than all scopes in \
          [prev_levels] (%a):@ %a"
        Scope.print current_scope
        Scope.print max_prev_scope
        print t
    end
*)
  end

let invariant t : unit = invariant0 t

let resolver t = t.resolver

let code_age_relation_resolver t =
  fun comp_unit ->
  match t.resolver comp_unit with
  | None -> None
  | Some t -> Some t.code_age_relation

let current_scope t = One_level.scope t.current_level

let names_to_types t =
  Cached.names_to_types (One_level.just_after_level t.current_level)

let aliases t =
  Cached.aliases (One_level.just_after_level t.current_level)

let create ~resolver ~get_imported_names =
  { resolver;
    get_imported_names;
    prev_levels = Scope.Map.empty;
    current_level = One_level.create_empty Scope.initial;
    next_binding_time = Binding_time.earliest_var;
    defined_symbols = Symbol.Set.empty;
    code_age_relation = Code_age_relation.empty;
    closure_env = Some {
      resolver;
      get_imported_names;
      prev_levels = Scope.Map.empty;
      current_level = One_level.create_empty Scope.initial;
      next_binding_time = Binding_time.earliest_var;
      defined_symbols = Symbol.Set.empty;
      code_age_relation = Code_age_relation.empty;
      closure_env = None;
    };
  }

let increment_scope t =
  let current_scope = current_scope t in
  let prev_levels =
    Scope.Map.add current_scope t.current_level t.prev_levels
  in
  let current_level =
    One_level.create (Scope.next current_scope) (Typing_env_level.empty ())
      ~just_after_level:(One_level.just_after_level t.current_level)
  in
  { t with
    prev_levels;
    current_level;
  }

let defined_symbols t = t.defined_symbols

let name_domain t =
  Name.Set.union (Name.Map.keys (names_to_types t))
    (Name.set_of_symbol_set (defined_symbols t))

let initial_symbol_type =
  lazy (Type_grammar.any_value (), Binding_time.symbols, Name_mode.normal)

let variable_is_from_missing_cmx_file t name =
  if Name.is_symbol name then false
  else
    let comp_unit = Name.compilation_unit name in
    if Compilation_unit.equal comp_unit (Compilation_unit.get_current_exn ())
    then false
    else
      match (resolver t) comp_unit with
      | exception _ -> true
      | None -> true
      | Some _ -> false

let check_optional_kind_matches name ty kind_opt =
  match kind_opt with
  | None -> ()
  | Some kind ->
    let ty_kind = Type_grammar.kind ty in
    if not (K.equal kind ty_kind) then begin
      Misc.fatal_errorf "Kind %a of type@ %a@ for %a@ \
          doesn't match expected kind %a"
        K.print ty_kind
        Type_grammar.print ty
        Name.print name
        K.print kind
    end

let find_with_binding_time_and_mode t name kind =
  match Name.Map.find name (names_to_types t) with
  | exception Not_found ->
    let comp_unit = Name.compilation_unit name in
    if Compilation_unit.equal comp_unit (Compilation_unit.get_current_exn ())
    then
      let [@inline always] var var =
        Misc.fatal_errorf "Variable %a not bound in typing environment:@ %a"
          Variable.print var
          print t
      in
      let [@inline always] symbol sym =
        if Symbol.Set.mem sym t.defined_symbols then
          let result = Lazy.force initial_symbol_type in
          check_optional_kind_matches name (Misc.fst3 result) kind;
          result
        else
          Misc.fatal_errorf "Symbol %a not bound in typing environment:@ %a"
            Symbol.print sym
            print t
      in
      Name.pattern_match name ~var ~symbol
    else
      begin match (resolver t) comp_unit with
      | exception _ ->
        Misc.fatal_errorf "Exception in resolver@ Backtrace is: %s"
          (Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()))
      | None ->
        Name.pattern_match name
          ~symbol:(fun _ ->  (* .cmx file missing *)
            let result = Lazy.force initial_symbol_type in
            check_optional_kind_matches name (Misc.fst3 result) kind;
            result)
          ~var:(fun _ ->
            match kind with
            | Some kind ->
              (* See comment below about binding times. *)
              Type_grammar.unknown kind, Binding_time.imported_variables,
                Name_mode.in_types
            | None ->
              Misc.fatal_errorf "Don't know kind of variable %a from another \
                  unit whose .cmx file is unavailable"
                Name.print name)
      | Some t ->
        match Name.Map.find name (names_to_types t) with
        | exception Not_found ->
          Name.pattern_match name
            ~symbol:(fun _ ->
              (* CR vlaviron: This could be an error, but it can actually occur
                 with predefined exceptions and maybe regular symbols too *)
              let result = Lazy.force initial_symbol_type in
              check_optional_kind_matches name (Misc.fst3 result) kind;
              result)
            ~var:(fun var ->
              Misc.fatal_errorf "Variable %a not bound in imported typing \
                  environment (maybe the wrong .cmx file is present?):@ %a"
                Variable.print var
                print t)
        | ty, _binding_time, name_mode ->
          check_optional_kind_matches name ty kind;
          (* Binding times for imported units are meaningless at present.
             Also see [Alias.defined_earlier]. *)
          ty, Binding_time.imported_variables, name_mode
      end
  | found ->
    let ty, _, _ = found in
    check_optional_kind_matches name ty kind;
    found

let find t name kind =
  let ty, _binding_time, _name_mode =
    find_with_binding_time_and_mode t name kind
  in
  ty

let find_params t params =
  List.map (fun param ->
      let name = Kinded_parameter.name param in
      let kind = Kinded_parameter.kind param in
    find t name (Some kind))
  params

let binding_time_and_mode t name =
  if variable_is_from_missing_cmx_file t name then
    Binding_time.With_name_mode.create
      Binding_time.imported_variables Name_mode.in_types
  else
    Name.pattern_match name
      ~var:(fun _var ->
        let _typ, binding_time, name_mode =
          find_with_binding_time_and_mode t name None
        in
        Binding_time.With_name_mode.create binding_time name_mode)
      ~symbol:(fun _sym ->
        Binding_time.With_name_mode.create
          Binding_time.symbols Name_mode.normal)

let binding_time_and_mode_of_simple t simple =
  Simple.pattern_match simple
    ~const:(fun _ ->
      Binding_time.With_name_mode.create
        Binding_time.consts_and_discriminants Name_mode.normal)
    ~name:(fun name -> binding_time_and_mode t name)

let mem t name =
  Name.pattern_match name
    ~var:(fun _var -> Name.Map.mem name (names_to_types t)
                      || Name.Set.mem name (t.get_imported_names ()))
    ~symbol:(fun sym -> Symbol.Set.mem sym t.defined_symbols)

let mem_simple t simple =
  Simple.pattern_match simple
    ~name:(fun name -> mem t name)
    ~const:(fun _ -> true)

let with_current_level t ~current_level =
  let t = { t with current_level; } in
  invariant t;
  t

let with_current_level_and_closure_env t ~current_level ~closure_env =
  let t = { t with current_level; closure_env; } in
  invariant t;
  t

let with_current_level_and_next_binding_time t ~current_level
      next_binding_time =
  let t = { t with current_level; next_binding_time; } in
  invariant t;
  t

let cached t = One_level.just_after_level t.current_level

let add_variable_definition t var kind name_mode =
  (* We can add equations in our own compilation unit on variables and
     symbols defined in another compilation unit. However we can't define other
     compilation units' variables or symbols (except for predefined
     symbols such as exceptions) in our own compilation unit. *)
  let comp_unit = Variable.compilation_unit var in
  let this_comp_unit = Compilation_unit.get_current_exn () in
  if not (Compilation_unit.equal comp_unit this_comp_unit) then begin
    Misc.fatal_errorf "Cannot define a variable that belongs to a different \
        compilation unit: %a@ in environment:@ %a"
      Variable.print var
      print t
  end;
  let name = Name.var var in
  if !Clflags.flambda_invariant_checks && mem t name then begin
    Misc.fatal_errorf "Cannot rebind %a in environment:@ %a"
      Name.print name
      print t
  end;
  let level =
    Typing_env_level.add_definition (One_level.level t.current_level) var kind
      t.next_binding_time
  in
  let just_after_level =
    Cached.add_or_replace_binding (cached t)
      name (Type_grammar.unknown kind)
      t.next_binding_time name_mode
      ~new_aliases:(aliases t)
  in
  let current_level =
    One_level.create (current_scope t) level ~just_after_level
  in
  with_current_level_and_next_binding_time t ~current_level
    (Binding_time.succ t.next_binding_time)

let rec add_symbol_definition t sym =
  (* CR mshinwell: check for redefinition when invariants enabled? *)
  let comp_unit = Symbol.compilation_unit sym in
  let this_comp_unit = Compilation_unit.get_current_exn () in
  if (not (Compilation_unit.equal comp_unit this_comp_unit)) then begin
    Misc.fatal_errorf "Cannot define symbol %a that belongs to a different \
        compilation unit@ (%a, current unit: %a) %b@ in environment:@ %a"
      Symbol.print sym
      Compilation_unit.print comp_unit
      Compilation_unit.print this_comp_unit
      (Compilation_unit.equal comp_unit this_comp_unit)
      print t
  end;
  let closure_env =
    match t.closure_env with
    | None -> None
    | Some closure_env -> Some (add_symbol_definition closure_env sym)
  in
  { t with
    defined_symbols = Symbol.Set.add sym t.defined_symbols;
    closure_env;
  }

let rec add_symbol_definitions t syms =
  let closure_env =
    match t.closure_env with
    | None -> None
    | Some closure_env -> Some (add_symbol_definitions closure_env syms)
  in
  { t with
    defined_symbols = Symbol.Set.union syms t.defined_symbols;
    closure_env;
  }

let kind_of_simple t simple =
  let [@inline always] const const =
    Type_grammar.kind (Type_grammar.type_for_const const)
  in
  let [@inline always] name name =
    (* [kind_of_simple] is only currently called when processing variables
       in terms, whose kinds should always be inferrable, so we pass
       [None] here. *)
    let ty = find t name None in
    Type_grammar.kind ty
  in
  Simple.pattern_match simple ~const ~name

let add_definition t (name : Name_in_binding_pos.t) kind =
  let name_mode = Name_in_binding_pos.name_mode name in
  Name.pattern_match (Name_in_binding_pos.name name)
    ~var:(fun var -> add_variable_definition t var kind name_mode)
    ~symbol:(fun sym ->
      if not (Name_mode.equal name_mode Name_mode.normal) then begin
        Misc.fatal_errorf "Cannot define symbol %a with name mode that \
            is not `normal'"
          Name_in_binding_pos.print name
      end;
      add_symbol_definition t sym)

let invariant_for_new_equation t name ty =
  if !Clflags.flambda_invariant_checks then begin
    (* CR mshinwell: This should check that precision is not decreasing. *)
    let defined_names =
      Name_occurrences.create_names
        (Name.Set.union (name_domain t) (t.get_imported_names ()))
        Name_mode.in_types
    in
    (* CR mshinwell: It's a shame we can't check code IDs here. *)
    let free_names =
      Name_occurrences.without_code_ids (Type_grammar.free_names ty)
    in
    if not (Name_occurrences.subset_domain free_names defined_names) then begin
      let unbound_names = Name_occurrences.diff free_names defined_names in
      Misc.fatal_errorf "New equation@ %a@ =@ %a@ has unbound names@ (%a):@ %a"
        Name.print name
        Type_grammar.print ty
        Name_occurrences.print unbound_names
        print t
    end
  end

let add_cse t prim ~bound_to =
  let level =
    Typing_env_level.add_cse (One_level.level t.current_level) prim ~bound_to
  in
  let cached = cached t in
  let cse = Cached.cse cached in
  match Flambda_primitive.Eligible_for_cse.Map.find prim cse with
  | exception Not_found ->
    let cse = Flambda_primitive.Eligible_for_cse.Map.add prim bound_to cse in
    let just_after_level = Cached.with_cse cached ~cse in
    let current_level =
      One_level.create (current_scope t) level ~just_after_level
    in
    with_current_level t ~current_level
  | _bound_to -> t

let rec add_equation0 t aliases name ty =
  if !Clflags.Flambda_2.Debug.concrete_types_only_on_canonicals then begin
    let is_concrete =
      match Type_grammar.get_alias_exn ty with
      | exception Not_found -> true
      | _ -> false
    in
    if is_concrete then begin
      let canonical = Aliases.get_canonical_ignoring_name_mode aliases name in
      if not (Simple.equal canonical (Simple.name name)) then begin
        Misc.fatal_errorf "Trying to add equation giving concrete type on %a \
            which is not canonical (its canonical is %a): %a"
          Name.print name
          Simple.print canonical
          Type_grammar.print ty
      end
    end
  end;
  invariant_for_new_equation t name ty;
  let level =
    Typing_env_level.add_or_replace_equation
      (One_level.level t.current_level) name ty
  in
  let just_after_level, closure_env =
    Name.pattern_match name
      ~var:(fun var ->
        let just_after_level =
          if Compilation_unit.equal (Variable.compilation_unit var)
               (Compilation_unit.get_current_exn ())
          then
            Cached.replace_variable_binding
              (One_level.just_after_level t.current_level)
              var ty ~new_aliases:aliases
          else
            Cached.add_or_replace_binding
              (One_level.just_after_level t.current_level)
              name ty Binding_time.imported_variables Name_mode.in_types
              ~new_aliases:aliases
        in
        just_after_level, t.closure_env)
      ~symbol:(fun _ ->
        let just_after_level =
          Cached.add_or_replace_binding
            (One_level.just_after_level t.current_level)
            name ty Binding_time.symbols Name_mode.normal
            ~new_aliases:aliases
        in
        let closure_env =
          match t.closure_env with
          | None -> None
          | Some closure_env ->
            let level, ty =
              Type_grammar.make_suitable_for_environment0 ty t
                ~suitable_for:closure_env (Typing_env_level.empty ())
            in
            let closure_env =
              add_equation (add_env_extension_from_level closure_env level)
                name ty
            in
            Some closure_env
        in
        just_after_level, closure_env)
  in
  let current_level =
    One_level.create (current_scope t) level ~just_after_level
  in
  with_current_level_and_closure_env t ~current_level ~closure_env

and add_equation t name ty =
  if !Clflags.flambda_invariant_checks then begin
    let existing_ty = find t name None in
    if not (K.equal (Type_grammar.kind existing_ty) (Type_grammar.kind ty))
    then begin
      Misc.fatal_errorf "Cannot add equation %a = %a@ given existing binding \
          %a = %a@ whose type is of a different kind:@ %a"
        Name.print name
        Type_grammar.print ty
        Name.print name
        Type_grammar.print existing_ty
        print t
    end
  (* XXX Needs to be guarded
  let free_names = Type_free_names.free_names ty in
  if not (Name_occurrences.subset_domain free_names (domain t))
  then begin
    let unbound_names = Name_occurrences.diff free_names (domain t) in
    Misc.fatal_errorf "Cannot add equation, involving unbound names@ (%a),@ on \
        name@ %a =@ %a@ (free names %a) in environment with domain %a:@ %a"
      Name_occurrences.print unbound_names
      Name.print name
      Type_grammar.print ty
      Name_occurrences.print free_names
      Name_occurrences.print (domain t)
      print t
  end;
  *)
  end;
  if !Clflags.flambda_invariant_checks then begin
    match Type_grammar.get_alias_exn ty with
    | exception Not_found -> ()
    | simple ->
      Simple.pattern_match simple
        ~name:(fun name' ->
          if Name.equal name name' then begin
            Misc.fatal_errorf "Directly recursive equation@ %a = %a@ \
                disallowed:@ %a"
              Name.print name
              Type_grammar.print ty
              print t
          end)
        ~const:(fun _ -> ())
  end;
  let aliases, simple, rec_info, t, ty =
    let aliases = aliases t in
    match Type_grammar.get_alias_exn ty with
    | exception Not_found ->
      (* Equations giving concrete types may only be added to the canonical
         element as known by the alias tracker (the actual canonical, ignoring
         any name modes). *)
      let canonical = Aliases.get_canonical_ignoring_name_mode aliases name in
      aliases, canonical, None, t, ty
    | alias_of ->
      let alias = Simple.name name in
      let kind = Type_grammar.kind ty in
      let binding_time_and_mode_alias = binding_time_and_mode t name in
      let rec_info = Simple.rec_info alias_of in
      let binding_time_and_mode_alias_of =
        binding_time_and_mode_of_simple t alias_of
      in
      let ({ canonical_element; alias_of; t = aliases; } : Aliases.add_result) =
        Aliases.add aliases alias binding_time_and_mode_alias
          alias_of binding_time_and_mode_alias_of
      in
      let ty =
        Type_grammar.alias_type_of kind canonical_element
      in
      aliases, alias_of, rec_info, t, ty
  in
  (* Beware: if we're about to add the equation on a name which is different
     from the one that the caller passed in, then we need to make sure that the
     type we assign to that name is the most precise available. This
     necessitates calling [meet].

     For example, suppose [p] is defined earlier than [x], with [p] of type
     [Unknown] and [x] of type [ty]. If the caller says that the best type of
     [p] is now to be "= x", then this function will add an equation on [x]
     rather than [p], due to the definition ordering. However we should not just
     say that [x] has type "= p", as that would forget any existing information
     about [x]. Instead we should say that [x] has type "(= p) meet ty".

     Note also that [p] and [x] may have different name modes! *)
  let ty, t =
    let [@inline always] name eqn_name =
      if Name.equal name eqn_name then ty, t
      else
        let env = Meet_env.create t in
        let existing_ty = find t eqn_name (Some (Type_grammar.kind ty)) in
        match Type_grammar.meet env ty existing_ty with
        | Bottom -> Type_grammar.bottom_like ty, t
        | Ok (meet_ty, env_extension) ->
          meet_ty, add_env_extension t env_extension
    in
    Simple.pattern_match simple ~name ~const:(fun _ -> ty, t)
  in
  let ty =
    match rec_info with
    | None -> ty
    | Some rec_info ->
      match Type_grammar.apply_rec_info ty rec_info with
      | Bottom -> Type_grammar.bottom (Type_grammar.kind ty)
      | Ok ty -> ty
  in
  let [@inline always] name name = add_equation0 t aliases name ty in
  Simple.pattern_match simple ~name ~const:(fun _ -> t)

and add_env_extension_from_level t level : t =
  let t =
    Typing_env_level.fold_on_defined_vars (fun var kind t ->
        add_variable_definition t var kind Name_mode.in_types)
      level
      t
  in
  let t =
    Name.Map.fold (fun name ty t -> add_equation t name ty)
      (Typing_env_level.equations level)
      t
  in
  Flambda_primitive.Eligible_for_cse.Map.fold (fun prim bound_to t ->
      add_cse t prim ~bound_to)
    (Typing_env_level.cse level)
    t

and add_env_extension t env_extension =
  let [@inline always] f level =
    (add_env_extension_from_level [@inlined never]) t level
  in
  Typing_env_extension.pattern_match env_extension ~f

(* These version is outside the [let rec] and thus does not cause
   [caml_apply*] to be used when calling from outside this module. *)
let add_equation t name ty = add_equation t name ty

let add_env_extension t env_extension = add_env_extension t env_extension

let add_definitions_of_params t ~params =
  List.fold_left (fun t param ->
      let name =
        Name_in_binding_pos.create (Kinded_parameter.name param)
          Name_mode.normal
      in
      add_definition t name (Kinded_parameter.kind param))
    t
    params

let add_equations_on_params t ~params ~param_types =
  if !Clflags.flambda_invariant_checks
    && List.compare_lengths params param_types <> 0
  then begin
    Misc.fatal_errorf "Mismatch between number of [params] and \
        [param_types]:@ (%a)@ and@ %a"
      Kinded_parameter.List.print params
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Type_grammar.print)
      param_types
  end;
  List.fold_left2 (fun t param param_type ->
      add_equation t (Kinded_parameter.name param) param_type)
    t
    params param_types

let find_cse t prim =
  let cse = Cached.cse (cached t) in
  match Flambda_primitive.Eligible_for_cse.Map.find prim cse with
  | exception Not_found -> None
  | bound_to -> Some bound_to

let add_to_code_age_relation t ~newer ~older =
  let code_age_relation =
    Code_age_relation.add t.code_age_relation ~newer ~older
  in
  { t with code_age_relation; }

let code_age_relation t = t.code_age_relation

let with_code_age_relation t code_age_relation =
  { t with code_age_relation; }

(* CR mshinwell: Change the name of the labelled argument *)
let cut t ~unknown_if_defined_at_or_later_than:min_scope =
  let current_scope = current_scope t in
  if Scope.(>) min_scope current_scope then
    Typing_env_level.empty ()
  else
    let _strictly_less, at_min_scope, strictly_greater =
      Scope.Map.split min_scope t.prev_levels
    in
    let at_or_after_cut =
      match at_min_scope with
      | None -> strictly_greater
      | Some typing_env_level ->
        Scope.Map.add min_scope typing_env_level strictly_greater
    in
    let at_or_after_cut =
      Scope.Map.add current_scope t.current_level at_or_after_cut
    in
    Scope.Map.fold (fun _scope one_level result ->
        Typing_env_level.concat result (One_level.level one_level))
      at_or_after_cut
      (Typing_env_level.empty ())

let cut_and_n_way_join definition_typing_env ts_and_use_ids ~params
      ~unknown_if_defined_at_or_later_than =
  (* CR mshinwell: Can't [unknown_if_defined_at_or_later_than] just be
     computed by this function? *)
  let after_cuts =
    List.map (fun (t, use_id, use_kind) ->
        let level = cut t ~unknown_if_defined_at_or_later_than in
        t, use_id, use_kind, level)
      ts_and_use_ids
  in
  let level, extra_params_and_args =
    Typing_env_level.n_way_join ~env_at_fork:definition_typing_env
      after_cuts ~params
  in
  let env_extension = Typing_env_extension.create level in
  env_extension, extra_params_and_args

let get_canonical_simple_with_kind_exn t ?min_name_mode simple =
  let kind = kind_of_simple t simple in
  let newer_rec_info =
    let newer_rec_info = Simple.rec_info simple in
    match newer_rec_info with
    | None -> None
    | Some newer_rec_info ->
      Simple.pattern_match simple
        ~const:(fun _ -> Some newer_rec_info)
        ~name:(fun name ->
          match Type_grammar.get_alias_exn (find t name (Some kind)) with
          | exception Not_found -> Some newer_rec_info
          | simple ->
            match Simple.rec_info simple with
            | None -> Some newer_rec_info
            | Some rec_info ->
              Some (Rec_info.merge rec_info ~newer:newer_rec_info))
  in
  let aliases_for_simple =
    if Aliases.mem (aliases t) simple then aliases t
    else
    Simple.pattern_match simple
      ~const:(fun _ -> aliases t)
      ~name:(fun name ->
        Name.pattern_match name
          ~var:(fun var ->
            let comp_unit = Variable.compilation_unit var in
            if Compilation_unit.equal comp_unit
                 (Compilation_unit.get_current_exn ())
            then aliases t
            else
              match (resolver t) comp_unit with
              | Some env -> aliases env
              | None ->
                Misc.fatal_errorf "Error while looking up variable %a:@ \
                                   No corresponding .cmx file was found"
                  Variable.print var)
          ~symbol:(fun _sym ->
            (* Symbols can't alias, so lookup in the current aliases is fine *)
            aliases t))
  in
  let name_mode_simple =
    Binding_time.With_name_mode.name_mode
      (binding_time_and_mode_of_simple t simple)
  in
  let min_name_mode =
    match min_name_mode with
    | None -> name_mode_simple
    | Some name_mode -> name_mode
  in
  match
    Aliases.get_canonical_element_exn aliases_for_simple simple name_mode_simple
      ~min_name_mode
  with
  | exception Misc.Fatal_error ->
    if !Clflags.flambda2_context_on_error then begin
      Format.eprintf "\n%sContext is:%s typing environment@ %a\n"
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        print t
    end;
    raise Misc.Fatal_error
  | alias ->
    match newer_rec_info with
    | None -> alias, kind
    | Some _ ->
      match Simple.merge_rec_info alias ~newer_rec_info with
      | None -> raise Not_found
      | Some simple -> simple, kind

let get_canonical_simple_exn t ?min_name_mode simple =
  (* Duplicated from above to eliminate the allocation of the returned pair. *)
  let newer_rec_info =
    let newer_rec_info = Simple.rec_info simple in
    match newer_rec_info with
    | None -> None
    | Some newer_rec_info ->
      Simple.pattern_match simple
        ~const:(fun _ -> Some newer_rec_info)
        ~name:(fun name ->
          if variable_is_from_missing_cmx_file t name then Some newer_rec_info
          else
            match Type_grammar.get_alias_exn (find t name None) with
            | exception Not_found -> Some newer_rec_info
            | simple ->
              match Simple.rec_info simple with
              | None -> Some newer_rec_info
              | Some rec_info ->
                Some (Rec_info.merge rec_info ~newer:newer_rec_info))
  in
  let aliases_for_simple =
    if Aliases.mem (aliases t) simple then aliases t
    else
    Simple.pattern_match simple
      ~const:(fun _ -> aliases t)
      ~name:(fun name ->
        Name.pattern_match name
          ~var:(fun var ->
            let comp_unit = Variable.compilation_unit var in
            if Compilation_unit.equal comp_unit
                 (Compilation_unit.get_current_exn ())
            then aliases t
            else
              match (resolver t) comp_unit with
              | Some env -> aliases env
              | None ->
                (* Transcript of Slack conversation relating to the next line:

                   mshinwell: @vlaviron Should it say "aliases t" perhaps?
                   There could be some weird cases here, e.g. if we are
                   building c.cmx and b.cmx is unavailable, but if b.cmx were
                   available it would tell us that this var is an alias to
                   something in a.cmx, which is available I'm concerned that
                   this could lead to not propagating a constraint, e.g. if
                   the var in c.cmx is found to be bottom, it should make the
                   one in a.cmx bottom too, but it won't as the chain is
                   broken. That could be observable if something else in
                   c.cmx directly points at a.cmx. Maybe this won't matter in
                   practice because the new type should always be more
                   precise, but I'm unsure. And what happens if it's not?

                   vlaviron: That's actually fine, I think: if you hide b.cmx,
                   then c.cmx does not know that the two variables are
                   aliased, so it will be less precise, but that's all Since
                   we've fixed Get_tag, I don't think loss of precision is a
                   soundness issue anymore And the new type should always be
                   the result of a meet with the previous type, I think, so
                   it should never be less precise For the aliases issue, I
                   think using aliases t is fine. It would only be a problem
                   if we had a way to learn later that the variable is
                   actually an alias, but that would only happen if for some
                   reason we later successfully load the missing cmx. *)
                  aliases t)
          ~symbol:(fun _sym ->
            (* Symbols can't alias, so lookup in the current aliases is fine *)
            aliases t))
  in
  let name_mode_simple =
    let in_types =
      Simple.pattern_match simple
        ~const:(fun _ -> false)
        ~name:(fun name -> variable_is_from_missing_cmx_file t name)
    in
    if in_types then Name_mode.in_types
    else
      Binding_time.With_name_mode.name_mode
        (binding_time_and_mode_of_simple t simple)
  in
  let min_name_mode =
    match min_name_mode with
    | None -> name_mode_simple
    | Some name_mode -> name_mode
  in
  match
    Aliases.get_canonical_element_exn aliases_for_simple simple name_mode_simple
      ~min_name_mode
  with
  | exception Misc.Fatal_error ->
    if !Clflags.flambda2_context_on_error then begin
      Format.eprintf "\n%sContext is:%s typing environment@ %a\n"
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        print t
    end;
    raise Misc.Fatal_error
  | alias ->
    match newer_rec_info with
    | None -> alias
    | Some _ ->
      match Simple.merge_rec_info alias ~newer_rec_info with
      | None -> raise Not_found
      | Some simple -> simple

let get_alias_then_canonical_simple_exn t ?min_name_mode typ =
  Type_grammar.get_alias_exn typ
  |> get_canonical_simple_exn t ?min_name_mode

let aliases_of_simple t ~min_name_mode simple =
  let aliases =
    Aliases.get_aliases (aliases t) simple
    |> Simple.Set.filter (fun alias ->
      let name_mode =
        Binding_time.With_name_mode.name_mode
          (binding_time_and_mode_of_simple t alias)
      in
      match Name_mode.compare_partial_order name_mode min_name_mode with
      | None -> false
      | Some c -> c >= 0)
  in
  let newer_rec_info = Simple.rec_info simple in
  match newer_rec_info with
  | None -> aliases
  | Some _ ->
    Simple.Set.fold (fun simple simples ->
        match Simple.merge_rec_info simple ~newer_rec_info with
        | None -> simples
        | Some simple -> Simple.Set.add simple simples)
      aliases
      Simple.Set.empty

let aliases_of_simple_allowable_in_types t simple =
  aliases_of_simple t ~min_name_mode:Name_mode.in_types simple

let closure_env t =
  match t.closure_env with
  | None ->
    create ~resolver:t.resolver ~get_imported_names:t.get_imported_names
  | Some closure_env ->
    assert (Option.is_none closure_env.closure_env);
    { closure_env with
      closure_env = Some {
        closure_env with
        closure_env = None;
      };
    }

let rec free_names_transitive_of_type_of_name t name ~result =
  let result = Name_occurrences.add_name result name Name_mode.in_types in
  if variable_is_from_missing_cmx_file t name then result
  else
    let typ = find t name None in
    free_names_transitive0 t typ ~result

and free_names_transitive0 t typ ~result =
  let free_names = Type_grammar.free_names typ in
  let to_traverse = Name_occurrences.diff free_names result in
  if Name_occurrences.is_empty to_traverse then result
  else
    Name_occurrences.fold_names to_traverse
      ~init:result
      ~f:(fun result name ->
        free_names_transitive_of_type_of_name t name ~result)

let free_names_transitive t typ =
  free_names_transitive0 t typ ~result:Name_occurrences.empty

let clean_for_export t ~module_symbol =
  let current_level =
    One_level.clean_for_export t.current_level ~module_symbol
  in
  { t with
    current_level;
  }
