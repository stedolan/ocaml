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

type t = {
  defined_vars : Flambda_kind.t Variable.Map.t;
  binding_times : Variable.Set.t Binding_time.Map.t;
  equations : Type_grammar.t Name.Map.t;
  cse : Simple.t Flambda_primitive.Eligible_for_cse.Map.t;
}

let defined_vars t = t.defined_vars

let defined_names t =
  Name.set_of_var_set (Variable.Map.keys t.defined_vars)

(*
let defines_name_but_no_equations t name =
  match Name.to_var name with
  | None -> false
  | Some var ->
    Variable.Map.mem var t.defined_vars
      && not (Name.Map.mem name t.equations)
*)

let print_with_cache ~cache ppf
      { defined_vars; binding_times = _; equations; cse; } =
  let print_equations ppf equations =
    let equations = Name.Map.bindings equations in
    match equations with
    | [] -> Format.pp_print_string ppf "()"
    | _::_ ->
      Format.pp_print_string ppf "(";
      Format.pp_print_list ~pp_sep:Format.pp_print_space
        (fun ppf (name, ty) ->
          Format.fprintf ppf
            "@[<hov 1>%a@ :@ %a@]"
            Name.print name
            (Type_grammar.print_with_cache ~cache) ty)
        ppf equations;
      Format.pp_print_string ppf ")"
  in
  (* CR mshinwell: Print [defined_vars] when not called from
     [Typing_env.print] *)
  if Variable.Map.is_empty defined_vars then
    if Flambda_primitive.Eligible_for_cse.Map.is_empty cse then
      Format.fprintf ppf
        "@[<hov 1>(\
          @[<hov 1>(equations@ @[<v 1>%a@])@])\
          @]"
        print_equations equations
    else
      Format.fprintf ppf
        "@[<hov 1>(\
          @[<hov 1>(equations@ @[<v 1>%a@])@])@ \
          @[<hov 1>(cse@ @[<hov 1>%a@])@]\
          @]"
        print_equations equations
        (Flambda_primitive.Eligible_for_cse.Map.print Simple.print) cse
  else
    if Flambda_primitive.Eligible_for_cse.Map.is_empty cse then
      Format.fprintf ppf
        "@[<hov 1>(\
          @[<hov 1>(equations@ @[<v 1>%a@])@]@ \
          )@]"
        print_equations equations
    else
      Format.fprintf ppf
        "@[<hov 1>(\
          @[<hov 1>(equations@ @[<v 1>%a@])@]@ \
          @[<hov 1>(cse@ @[<hov 1>%a@])@]\
          )@]"
        print_equations equations
        (Flambda_primitive.Eligible_for_cse.Map.print Simple.print) cse

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant _t = ()

let fold_on_defined_vars f t init =
  Binding_time.Map.fold (fun _bt vars acc ->
      Variable.Set.fold (fun var acc ->
          let kind = Variable.Map.find var t.defined_vars in
          f var kind acc)
        vars
        acc)
    t.binding_times
    init

let apply_name_permutation ({ defined_vars; binding_times; equations; cse; } as t)
      perm =
  let defined_vars_changed = ref false in
  let defined_vars' =
    Variable.Map.fold (fun var kind defined_vars ->
        let var' = Name_permutation.apply_variable perm var in
        if not (var == var') then begin
          defined_vars_changed := true
        end;
        Variable.Map.add var' kind defined_vars)
      defined_vars
      Variable.Map.empty
  in
  let binding_times' =
    if !defined_vars_changed then
      Binding_time.Map.fold (fun binding_time vars binding_times ->
          let vars' =
            Variable.Set.map (fun var ->
              Name_permutation.apply_variable perm var)
              vars
          in
          Binding_time.Map.add binding_time vars' binding_times)
        binding_times
        Binding_time.Map.empty
    else
      binding_times
  in
  let equations_changed = ref false in
  let equations' =
    Name.Map.fold (fun name typ equations ->
        let name' = Name_permutation.apply_name perm name in
        let typ' = Type_grammar.apply_name_permutation typ perm in
        if not (name == name' && typ == typ') then begin
          equations_changed := true
        end;
        Name.Map.add name' typ' equations)
      equations
      Name.Map.empty
  in
  let cse_changed = ref false in
  let cse' =
    Flambda_primitive.Eligible_for_cse.Map.fold (fun prim simple cse' ->
        let simple' = Simple.apply_name_permutation simple perm in
        let prim' =
          Flambda_primitive.Eligible_for_cse.apply_name_permutation prim perm
        in
        if (not (simple == simple')) || (not (prim == prim')) then begin
          cse_changed := true
        end;
        Flambda_primitive.Eligible_for_cse.Map.add prim' simple' cse')
      cse
      Flambda_primitive.Eligible_for_cse.Map.empty
  in
  if (not !defined_vars_changed)
    && (not !equations_changed)
    && (not !cse_changed)
  then t
  else 
    { defined_vars = defined_vars';
      binding_times = binding_times';
      equations = equations';
      cse = cse';
    }

let free_names { defined_vars; binding_times = _; equations; cse; } =
  let free_names_defined_vars =
    Name_occurrences.create_variables (Variable.Map.keys defined_vars)
      Name_mode.in_types
  in
  let free_names_equations =
    Name.Map.fold (fun name typ free_names ->
        let free_names' = 
          Name_occurrences.add_name (Type_grammar.free_names typ)
            name Name_mode.in_types
        in
        Name_occurrences.union free_names free_names')
      equations
      free_names_defined_vars
  in
  Flambda_primitive.Eligible_for_cse.Map.fold
    (fun prim (bound_to : Simple.t) acc ->
      Simple.pattern_match bound_to
        ~const:(fun _ -> acc)
        ~name:(fun name ->
          let free_in_prim =
            Name_occurrences.downgrade_occurrences_at_strictly_greater_kind
              (Flambda_primitive.Eligible_for_cse.free_names prim)
              Name_mode.in_types
          in
          Name_occurrences.add_name free_in_prim
            name Name_mode.in_types))
    cse
    free_names_equations

let empty () =
  { defined_vars = Variable.Map.empty;
    binding_times = Binding_time.Map.empty;
    equations = Name.Map.empty;
    cse = Flambda_primitive.Eligible_for_cse.Map.empty;
  }

let is_empty { defined_vars; binding_times; equations; cse; } =
  Variable.Map.is_empty defined_vars
    && Binding_time.Map.is_empty binding_times
    && Name.Map.is_empty equations
    && Flambda_primitive.Eligible_for_cse.Map.is_empty cse

let equations t = t.equations

let cse t = t.cse

let add_definition t var kind binding_time =
  if !Clflags.flambda_invariant_checks
    && Variable.Map.mem var t.defined_vars
  then begin
    Misc.fatal_errorf "Environment extension already binds variable %a:@ %a"
      Variable.print var
      print t
  end;
  let binding_times =
    let vars =
      match Binding_time.Map.find binding_time t.binding_times with
      | exception Not_found ->
        Variable.Set.singleton var
      | prev_vars ->
        Variable.Set.add var prev_vars
    in
    Binding_time.Map.add binding_time vars t.binding_times
  in
  { t with
    defined_vars = Variable.Map.add var kind t.defined_vars;
    binding_times;
  }

let equation_is_directly_recursive name ty =
  match Type_grammar.get_alias_exn ty with
  | exception Not_found -> false
  | simple ->
    Simple.pattern_match simple
      ~name:(fun name' -> Name.equal name name')
      ~const:(fun _ -> false)

let check_equation t name ty =
  if !Clflags.flambda_invariant_checks then begin
    if equation_is_directly_recursive name ty then begin
      Misc.fatal_errorf "Directly recursive equation@ %a = %a@ \
          disallowed (Typing_env_level):@ %a"
        Name.print name
        Type_grammar.print ty
        print t
    end
  end

let one_equation name ty =
  check_equation (empty ()) name ty;
  { defined_vars = Variable.Map.empty;
    binding_times = Binding_time.Map.empty;
    equations = Name.Map.singleton name ty;
    cse = Flambda_primitive.Eligible_for_cse.Map.empty;
  }

let add_or_replace_equation t name ty =
  check_equation t name ty;
  if Type_grammar.is_obviously_unknown ty then
    { t with
      equations = Name.Map.remove name t.equations;
    }
  else
    { t with
      equations = Name.Map.add name ty t.equations;
    }

let add_cse t prim ~bound_to =
  match Flambda_primitive.Eligible_for_cse.Map.find prim t.cse with
  | exception Not_found ->
    let cse =
      Flambda_primitive.Eligible_for_cse.Map.add prim bound_to t.cse
    in
    { t with cse; }
  | _bound_to -> t

let concat (t1 : t) (t2 : t) =
  let defined_vars =
    Variable.Map.union (fun var _data1 _data2 ->
        Misc.fatal_errorf "Cannot concatenate levels that have overlapping \
            defined variables (e.g. %a):@ %a@ and@ %a"
          Variable.print var
          print t1
          print t2)
      t1.defined_vars
      t2.defined_vars
  in
  let binding_times =
    Binding_time.Map.union (fun _binding_time vars1 vars2 ->
      (* CR vlaviron: Technically this is feasible, as we can allow several
         variables with the same binding time, but it should only come from
         joins; concat arguments should always have disjoint binding time
         domains *)
        Misc.fatal_errorf "Cannot concatenate levels that have variables \
            with overlapping binding times (e.g. %a and %a):@ %a@ and@ %a"
          Variable.Set.print vars1
          Variable.Set.print vars2
          print t1
          print t2)
      t1.binding_times
      t2.binding_times
  in
  let equations =
    Name.Map.union (fun _ _ty1 ty2 -> Some ty2) t1.equations t2.equations
  in
  let cse =
    Flambda_primitive.Eligible_for_cse.Map.union (fun _prim _t1 t2 -> Some t2)
      t1.cse
      t2.cse
  in
  { defined_vars;
    binding_times;
    equations;
    cse;
  }

let meet_equation0 env t name typ =
  check_equation t name typ;
  let meet_typ, env_extension =
    match Name.Map.find name t.equations with
    | exception Not_found -> typ, Typing_env_extension.empty ()
    | existing_typ -> Type_grammar.meet' env typ existing_typ
  in
  let env =
    let typing_env =
      let typing_env = Meet_env.env env in
      Typing_env.add_equation
        (Typing_env.add_env_extension typing_env env_extension)
        name typ
    in
    Meet_env.with_typing_env env typing_env
  in
  (* CR mshinwell: This special case needs further thinking about *)
  (* When meeting recursive types we can end up attempting to add
     equations "x : =x". *)
  let equations =
    if equation_is_directly_recursive name meet_typ then t.equations
    else Name.Map.add (* replace *) name meet_typ t.equations
  in
  let equations =
    Typing_env_extension.pattern_match env_extension ~f:(fun t_from_meet ->
      if not (Variable.Map.is_empty t_from_meet.defined_vars) then begin
        Misc.fatal_errorf "Didn't expect [defined_vars] in:@ %a"
          print t_from_meet
      end;
      Name.Map.fold (fun name typ equations ->
          check_equation t name typ;
          Name.Map.add (* replace *) name typ equations)
        t_from_meet.equations
        equations)
  in
  let t = { t with equations; } in
  t, env

let meet_equation env t name typ =
  try meet_equation0 env t name typ
  with Misc.Fatal_error ->
    if !Clflags.flambda2_context_on_error then begin
      Format.eprintf "\n%sContext is:%s meeting equation %a : %a@ in \
          level@ %a@ and environment@ %a\n"
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        Name.print name
        Type_grammar.print typ
        print t
        Typing_env.print (Meet_env.env env)
    end;
    raise Misc.Fatal_error

let meet0 env (t1 : t) (t2 : t) =
  let defined_vars =
    Variable.Map.union (fun var kind1 kind2 ->
        if Flambda_kind.equal kind1 kind2 then Some kind1
        else
          Misc.fatal_errorf "Cannot meet levels that have overlapping \
              defined variables (e.g. %a) that disagree on kind and/or \
              binding time:@ %a@ and@ %a"
            Variable.print var
            print t1
            print t2)
      t1.defined_vars
      t2.defined_vars
  in
  let binding_times =
    Binding_time.Map.union (fun _bt vars1 vars2 ->
        Some (Variable.Set.union vars1 vars2))
      t1.binding_times
      t2.binding_times
  in
  let env =
    let typing_env =
      (* Iterating on binding_times ensures that the resulting typing env
         is compatible with both inputs regarding binding times.
         When several variables have the same binding time, we assume they
         come from distinct contexts and that their relative ordering does not
         matter. *)
      Binding_time.Map.fold (fun _bt vars typing_env ->
          Variable.Set.fold (fun var typing_env ->
              let kind = Variable.Map.find var defined_vars in
              let name =
                Name_in_binding_pos.create (Name.var var) Name_mode.in_types
              in
              Typing_env.add_definition typing_env name kind)
            vars
            typing_env)
        binding_times
        (Meet_env.env env)
    in
    Meet_env.with_typing_env env typing_env
  in
  let t =
    { (empty ()) with
      defined_vars;
      binding_times;
    }
  in
  let t, env =
    Name.Map.fold (fun name ty (t, env) -> meet_equation env t name ty)
      t1.equations
      (t, env)
  in
  let t, _env =
    Name.Map.fold (fun name ty (t, env) -> meet_equation env t name ty)
      t2.equations
      (t, env)
  in
  let cse =
    (* CR mshinwell: Add [Map.inter] (also used elsewhere) *)
    Flambda_primitive.Eligible_for_cse.Map.merge (fun _ simple1 simple2 ->
        match simple1, simple2 with
        | None, None | None, Some _ | Some _, None -> None
        | Some simple1, Some simple2 ->
          if Simple.equal simple1 simple2 then Some simple1
          else None)
      t1.cse t2.cse
  in
  { t with cse; }

let meet env t1 t2 =
  (* Care: the domains of [t1] and [t2] are treated as contravariant.
     As such, since this is [meet], we perform unions on the domains.
     So if one of them is bottom, the result of meeting it with any other
     level is that level, not bottom. *)
  if is_empty t1 then t2
  else if is_empty t2 then t1
  else meet0 env t1 t2

(* CR mshinwell: [env_at_fork] -> [env_at_cut] *)

let join_types ~env_at_fork ~params envs_with_levels =
  let symbols_at_fork = Typing_env.defined_symbols env_at_fork in
  (* let joined_types =
   *   (\* The [defined_vars] in the different levels may overlap. *\)
   *   List.fold_left (fun joined_types (_env_at_use, _id, _use_kind, t) ->
   *       Variable.Map.fold (fun var kind joined_types ->
   *           Name.Map.add (Name.var var) (Type_grammar.bottom kind) joined_types)
   *         t.defined_vars
   *         joined_types)
   *     Name.Map.empty
   *     envs_with_levels
   * in *)
  let envs_with_levels =
    List.map (fun (env_at_use, id, use_kind, t) ->
        let symbols_defined_during_level =
          Symbol.Set.diff (Typing_env.defined_symbols env_at_use)
            symbols_at_fork
        in
        env_at_use, id, use_kind, t, symbols_defined_during_level)
      envs_with_levels
  in
  (* let joined_types =
   *   List.fold_left
   *     (fun joined_types
   *          (_env_at_use, _id, _use_kind, _t, symbols_defined_during_level) ->
   *       Symbol.Set.fold (fun symbol joined_types ->
   *           Name.Map.add (Name.symbol symbol)
   *             (Type_grammar.bottom Flambda_kind.value)
   *             joined_types)
   *         symbols_defined_during_level
   *         joined_types)
   *     joined_types
   *     envs_with_levels
   * in *)
  (* let joined_types =
   *   List.fold_left (fun joined_types param ->
   *       Name.Map.add (Kinded_parameter.name param)
   *         (Type_grammar.bottom (Kinded_parameter.kind param))
   *         joined_types)
   *     joined_types
   *     params
   * in *)
  let env_at_fork =
    List.fold_left
      (fun env_at_fork
           (_env_at_use, _id, _use_kind, _t, symbols_defined_during_level) ->
        Typing_env.add_symbol_definitions env_at_fork
          symbols_defined_during_level)
      env_at_fork
      envs_with_levels
  in
  let (joined_types, code_age_relation, _) =
  List.fold_left
    (fun (joined_types, code_age_relation, is_first_join)
         (env_at_use, _id, _use_kind, t, symbols_defined_during_level) ->
      (*
      Format.eprintf "join_types with level:@ %a\n%!" print t;
      *)
      let code_age_relation =
        Code_age_relation.union code_age_relation
          (Typing_env.code_age_relation env_at_use)
      in
      let left_env =
        let level =
          { defined_vars = Variable.Map.empty;
            binding_times = Binding_time.Map.empty;
            equations = joined_types;
            cse = Flambda_primitive.Eligible_for_cse.Map.empty;
          }
        in
        let env_extension = Typing_env_extension.create level in
        let left_env = Typing_env.add_env_extension env_at_fork env_extension in
        Typing_env.with_code_age_relation left_env code_age_relation
      in
      let equations =
        (* If the parameter is not present, it is considered as bottom
           by the Name.Map.union while it should be unknown.
           By default non present equations are unknown, but here we must
           make it explicit. *)
        List.fold_left (fun equations param ->
          let param_name = Kinded_parameter.name param in
          if not (Name.Map.mem param_name equations) then
            Name.Map.add param_name
              (Type_grammar.unknown (Kinded_parameter.kind param))
              equations
          else
            equations)
          t.equations
          params
      in
      let equations =
        (* Same as just above, but for symbols. *)
        Symbol.Set.fold (fun symbol equations ->
            let name = Name.symbol symbol in
            if not (Name.Map.mem name equations) then
              Name.Map.add name (Type_grammar.any_value ()) equations
            else
              equations)
          symbols_defined_during_level
          equations
      in
      let equations =
        Variable.Map.fold (fun var kind equations ->
          let name = Name.var var in
          if not (Name.Map.mem name equations) then
            Name.Map.add name (Type_grammar.unknown kind) equations
          else
            equations)
          t.defined_vars
          equations
      in
      let joined_types =
        Name.Map.merge (fun name joined_ty use_ty ->
            (*
            Format.eprintf "Processing name:@ %a\n%!" Name.print name;
            *)
            match joined_ty, use_ty with
            | None, None -> assert false
            | Some joined_ty, Some use_ty ->
              let joined_ty =
                (* Recall: the order of environments matters here. *)
                Type_grammar.join ~bound_name:name
                  env_at_fork
                  ~left_env ~left_ty:joined_ty
                  ~right_env:env_at_use ~right_ty:use_ty
              in
              (*
                 Format.eprintf "joined type:@ %a\n%!"
                 Type_grammar.print joined_ty;
              *)
              Some joined_ty
            | Some joined_ty, None ->
              let joined_ty =
                if Typing_env.mem env_at_fork name then
                  Type_grammar.join ~bound_name:name
                    env_at_fork
                    ~left_env ~left_ty:joined_ty
                    ~right_env:env_at_use
                    ~right_ty:(Type_grammar.unknown_like joined_ty)
                else
                  joined_ty
              in
              Some joined_ty
            | None, Some use_ty ->
              let joined_ty =
                if Typing_env.mem env_at_fork name && not is_first_join then
                  Type_grammar.join ~bound_name:name
                    env_at_fork
                    ~left_env ~left_ty:(Type_grammar.unknown_like use_ty)
                    ~right_env:env_at_use ~right_ty:use_ty
                else
                  use_ty
              in
              Some joined_ty
        )
          joined_types
          equations
      in
      joined_types, code_age_relation, false)
    (Name.Map.empty, Typing_env.code_age_relation env_at_fork, true)
    envs_with_levels
  in
  joined_types, code_age_relation

module Rhs_kind = struct
  type t =
    | Needs_extra_binding of { bound_to : Simple.t; }
    | Rhs_in_scope of { bound_to : Simple.t; }

  let bound_to t =
    match t with
    | Needs_extra_binding { bound_to; }
    | Rhs_in_scope { bound_to; } -> bound_to

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Needs_extra_binding { bound_to; } ->
        Format.fprintf ppf "@[<hov 1>(Needs_extra_binding@ %a)@]"
          Simple.print bound_to
      | Rhs_in_scope { bound_to; } ->
        Format.fprintf ppf "@[<hov 1>(Rhs_in_scope@ %a)@]"
          Simple.print bound_to

    let output _ _ = Misc.fatal_error "Rhs_kind.output not yet implemented"
    let hash _ = Misc.fatal_error "Rhs_kind.hash not yet implemented"
    let equal _ = Misc.fatal_error "Rhs_kind.equal not yet implemented"

    let compare t1 t2 =
      match t1, t2 with
      | Needs_extra_binding { bound_to = bound_to1; },
          Needs_extra_binding { bound_to = bound_to2; } ->
        Simple.compare bound_to1 bound_to2
      | Rhs_in_scope { bound_to = bound_to1; },
          Rhs_in_scope { bound_to = bound_to2; } ->
        Simple.compare bound_to1 bound_to2
      | Needs_extra_binding _, _ -> -1
      | Rhs_in_scope _, _ -> 1
  end)
end

let cse_with_eligible_lhs ~env_at_fork envs_with_levels ~params prev_cse
      (extra_bindings: Continuation_extra_params_and_args.t) extra_equations =
  let module EP = Flambda_primitive.Eligible_for_cse in
  let params = Kinded_parameter.List.simple_set params in
  List.fold_left (fun eligible (env_at_use, id, _, t) ->
      let find_new_name =
        if Continuation_extra_params_and_args.is_empty extra_bindings
        then (fun _arg -> None)
        else begin
          let extra_args =
            Apply_cont_rewrite_id.Map.find id
              extra_bindings.extra_args
          in
          let rec find_name simple params args =
            match args, params with
            | [], [] -> None
            | [], _ | _, [] ->
              Misc.fatal_error "Mismatching params and args arity"
            | arg :: args, param :: params ->
              begin
              match (arg : Continuation_extra_params_and_args.Extra_arg.t) with
              | Already_in_scope arg when Simple.equal arg simple ->
                (* If [param] has an extra equation associated to it,
                   we shouldn't propagate equations on it as it will mess
                   with the application of constraints later *)
                if Name.Map.mem (Kinded_parameter.name param) extra_equations
                then None
                else Some (Kinded_parameter.simple param)
              | Already_in_scope _ | New_let_binding _ ->
                find_name simple params args
              end
          in
          (fun arg -> find_name arg extra_bindings.extra_params extra_args)
        end
      in
      EP.Map.fold (fun prim bound_to eligible ->
        let prim =
          EP.filter_map_args prim ~f:(fun arg ->
            match
              Typing_env.get_canonical_simple_exn env_at_use arg
                ~min_name_mode:Name_mode.normal
            with
            | exception Not_found -> None
            | arg ->
              begin match find_new_name arg with
              | None ->
                if Typing_env.mem_simple env_at_fork arg
                then Some arg
                else None
              | Some _ as arg_opt -> arg_opt
              end)
        in
        match prim with
        | None -> eligible
        | Some prim when EP.Map.mem prim prev_cse ->
          (* We've already got it from a previous round *)
          eligible
        | Some prim ->
          match
            Typing_env.get_canonical_simple_exn env_at_use bound_to
              ~min_name_mode:Name_mode.normal
          with
          | exception Not_found -> eligible
          | bound_to ->
            let bound_to =
              (* CR mshinwell: Think about whether this is the best fix.
                 The canonical simple might end up being one of the [params]
                 since they are defined in [env_at_fork].  However these
                 aren't bound at the use sites, so we must choose another
                 alias that is. *)
              if not (Simple.Set.mem bound_to params) then Some bound_to
              else
                let aliases =
                  Typing_env.aliases_of_simple env_at_use
                    ~min_name_mode:Name_mode.normal bound_to
                  |> Simple.Set.filter (fun simple ->
                    not (Simple.Set.mem simple params))
                in
                Simple.Set.get_singleton aliases
            in
            match bound_to with
            | None -> eligible
            | Some bound_to ->
              let bound_to : Rhs_kind.t =
                if Typing_env.mem_simple env_at_fork bound_to then
                  Rhs_in_scope { bound_to; }
                else
                  Needs_extra_binding { bound_to; }
              in
              (* CR mshinwell: Add [Map.add_or_replace]. *)
              match EP.Map.find prim eligible with
              | exception Not_found ->
                EP.Map.add prim
                  (Apply_cont_rewrite_id.Map.singleton id bound_to)
                  eligible
              | from_prev_levels ->
                let map =
                  Apply_cont_rewrite_id.Map.add id bound_to from_prev_levels
                in
                EP.Map.add prim map eligible)
      t.cse
      eligible)
    EP.Map.empty
    envs_with_levels

let join_cse envs_with_levels cse ~allowed =
  let module EP = Flambda_primitive.Eligible_for_cse in
  EP.Map.fold (fun prim bound_to_map
                (cse, extra_bindings, extra_equations, allowed) ->
      let has_value_on_all_paths =
        List.for_all (fun (_, id, _, _) ->
            Apply_cont_rewrite_id.Map.mem id bound_to_map)
          envs_with_levels
      in
      if not has_value_on_all_paths then
        cse, extra_bindings, extra_equations, allowed
      else
        let bound_to_set =
          Apply_cont_rewrite_id.Map.data bound_to_map
          |> Rhs_kind.Set.of_list
        in
        match Rhs_kind.Set.get_singleton bound_to_set with
        | Some (Rhs_kind.Rhs_in_scope { bound_to; }) ->
          EP.Map.add prim bound_to cse, extra_bindings, extra_equations, allowed
        | None | Some (Rhs_kind.Needs_extra_binding { bound_to = _; }) ->
          let prim_result_kind =
            Flambda_primitive.result_kind' (EP.to_primitive prim)
          in
          let var = Variable.create "cse_param" in
          let extra_param = Kinded_parameter.create var prim_result_kind in
          let bound_to =
            Apply_cont_rewrite_id.Map.map Rhs_kind.bound_to bound_to_map
          in
          let cse = EP.Map.add prim (Simple.var var) cse in
          let extra_args =
            Apply_cont_rewrite_id.Map.map
              (fun simple : Continuation_extra_params_and_args.Extra_arg.t ->
                Already_in_scope simple)
              bound_to
          in
          let extra_bindings =
            Continuation_extra_params_and_args.add extra_bindings
              ~extra_param ~extra_args
          in
          let extra_equations =
            (* For the primitives Is_int and Get_tag, they're strongly linked
               to their argument: additional information on the cse parameter
               should translate into additional information on the argument.
               This can be done by giving them the appropriate type. *)
            match EP.to_primitive prim with
            | Unary (Is_int, scrutinee) ->
              Name.Map.add
                (Name.var var) (Type_grammar.is_int_for_scrutinee ~scrutinee)
                extra_equations
            | Unary (Get_tag, block) ->
              Name.Map.add
                (Name.var var) (Type_grammar.get_tag_for_block ~block)
                extra_equations
            | _ -> extra_equations
          in
          let allowed =
            Name_occurrences.add_name allowed (Name.var var)
              Name_mode.normal
          in
          cse, extra_bindings, extra_equations, allowed)
    cse
    (EP.Map.empty,
     Continuation_extra_params_and_args.empty,
     Name.Map.empty,
     allowed)

let construct_joined_level envs_with_levels ~allowed ~joined_types ~cse =
  let module EP = Flambda_primitive.Eligible_for_cse in
  let defined_vars, binding_times =
    List.fold_left (fun (defined_vars, binding_times)
                     (_env_at_use, _id, _use_kind, t) ->
        let defined_vars_this_level =
          Variable.Map.filter (fun var _ ->
              Name_occurrences.mem_var allowed var)
            t.defined_vars
        in
        let defined_vars =
          Variable.Map.union (fun var kind1 kind2 ->
              if Flambda_kind.equal kind1 kind2 then Some kind1
              else
                Misc.fatal_errorf "Cannot join levels that disagree on the kind \
                    of [defined_vars] (%a and %a for %a)"
                  Flambda_kind.print kind1
                  Flambda_kind.print kind2
                  Variable.print var)
            defined_vars
            defined_vars_this_level
        in
        let binding_times_this_level =
          Binding_time.Map.filter_map t.binding_times
            ~f:(fun _ vars ->
              let vars =
                Variable.Set.filter (fun var ->
                    Name_occurrences.mem_var allowed var)
                  vars
              in
              if Variable.Set.is_empty vars then None
              else Some vars)
        in
        let binding_times =
          Binding_time.Map.union (fun _bt vars1 vars2 ->
              Some (Variable.Set.union vars1 vars2))
            binding_times
            binding_times_this_level
        in
        (defined_vars, binding_times))
      (Variable.Map.empty, Binding_time.Map.empty)
      envs_with_levels
  in
  let equations =
    Name.Map.filter (fun name _ty -> Name_occurrences.mem_name allowed name)
      joined_types
  in
  let cse =
    (* Any CSE equation whose right-hand side identifies a name in the [allowed]
       set is propagated.  We don't need to check the left-hand sides because
       we know all of those names are in [env_at_fork]. *)
    EP.Map.filter (fun _prim bound_to ->
        Simple.pattern_match bound_to
          ~const:(fun _ -> true)
          ~name:(fun name -> Name_occurrences.mem_name allowed name))
      cse
  in
  { defined_vars;
    binding_times;
    equations;
    cse;
  }

let join ~env_at_fork envs_with_levels ~params =
  let module EP = Flambda_primitive.Eligible_for_cse in
  (*
  Format.eprintf "JOIN\n%!";
  Format.eprintf "At fork:@ %a\n%!" Typing_env.print env_at_fork;
  List.iter (fun (_, _, _, t) ->
      Format.eprintf "One level:@ %a\n%!" print t)
    envs_with_levels;
  *)
  (* Add all the variables defined by the branches as existentials.
     Iterating on binding_times instead of defined_vars ensures
     consistency of binding time order in the branches and the result. *)
  let env_at_fork_with_existentials_defined =
    List.fold_left (fun env_at_fork (_, _, _, level) ->
        Binding_time.Map.fold (fun _ vars env ->
            Variable.Set.fold (fun var env ->
                if Typing_env.mem env (Name.var var) then env
                else
                  let kind = Variable.Map.find var level.defined_vars in
                  Typing_env.add_definition env
                    (Name_in_binding_pos.var
                       (Var_in_binding_pos.create var Name_mode.in_types))
                    kind)
              vars
              env)
          level.binding_times
          env_at_fork)
      env_at_fork
      envs_with_levels
  in
  (* For each name which has an equation on at least one of the given levels,
     calculate the joined type for that name, across all the given levels.
     If no type exists on a given level then it is treated as bottom (which
     does not affect the joined type being calculated). *)
  let joined_types, _ =
    join_types ~env_at_fork:env_at_fork_with_existentials_defined
      envs_with_levels ~params
  in
  (*
  Format.eprintf "joined_types:@ %a\n%!"
    (Name.Map.print Type_grammar.print) joined_types;
  *)
  (* Next calculate which equations (describing joined types) to propagate to
     the join point.  (Recall that the environment at the fork point includes
     the parameters of the continuation being called at the join. We wish to
     ensure that information in the types of these parameters is not lost.)
     - Equations on names defined in the environment at the fork point are
     always propagated.
     - Definitions of, and equations on, names that occur free on the
     right-hand sides of the propagated equations are also themselves
     propagated. The definition of any such propagated name (i.e. one that
     does not occur in the environment at the fork point) will be made
     existential. *)
  (*
  Format.eprintf "ENV WITH EXISTENTIALS:@ %a\n%!"
    Typing_env.print env_at_fork_with_existentials_defined;
  *)
  (* CR vlaviron: We need to compute the free names of joined_types,
     we can't use a typing environment *)
  let free_names_transitive typ =
    let rec free_names_transitive0 typ ~result =
      let free_names = Type_grammar.free_names typ in
      let to_traverse = Name_occurrences.diff free_names result in
      Name_occurrences.fold_names to_traverse
        ~init:result
        ~f:(fun result name ->
          match Name.Map.find name joined_types with
          | exception Not_found -> result
          | typ ->
            let result =
              Name_occurrences.add_name result name Name_mode.in_types
            in
            free_names_transitive0 typ ~result)
    in
    free_names_transitive0 typ ~result:Name_occurrences.empty
  in
  let allowed =
    Name.Map.fold (fun name ty allowed ->
        (*
        Format.eprintf "Processing %a : %a\n%!"
          Name.print name Type_grammar.print ty;
        *)
        if Typing_env.mem env_at_fork name
          || Name.is_symbol name
        then
          Name_occurrences.add_name
            (Name_occurrences.union allowed
              (free_names_transitive ty))
            name Name_mode.in_types
        else
          allowed)
      joined_types
      Name_occurrences.empty
  in
  (*
  Format.eprintf "allowed (1):@ %a\n%!" Name_occurrences.print allowed;
  *)
  let compute_cse_one_round prev_cse extra_params extra_equations allowed =
  (* CSE equations have a left-hand side specifying a primitive and a
     right-hand side specifying a [Simple].  The left-hand side is matched
     against portions of terms.  As such, the [Simple]s therein must have
     name mode [Normal], since we do not do CSE for phantom bindings (see
     [Simplify_common]).  It follows that any CSE equation whose left-hand side
     involves a name not defined at the fork point, having canonicalised such
     name, cannot be propagated.  This step also canonicalises the right-hand
     sides of the CSE equations. *)
  (*
  Format.eprintf "params:@ %a\n%!" Kinded_parameter.List.print params;
  *)
    let new_cse =
      cse_with_eligible_lhs ~env_at_fork envs_with_levels ~params
        prev_cse extra_params extra_equations
    in
  (*
  Format.eprintf "CSE with eligible LHS:@ %a\n%!"
    (Flambda_primitive.Eligible_for_cse.Map.print
      (Apply_cont_rewrite_id.Map.print Rhs_kind.print))
    cse;
  *)
  (* To make use of a CSE equation at or after the join point, its right-hand
     side must have the same value, no matter which path is taken from the
     fork point to the join point.  We filter out equations that do not
     satisfy this.  Sometimes we can force an equation to satisfy the
     property by explicitly passing the value of the right-hand side as an
     extra parameter to the continuation at the join point. *)
    let cse', extra_params', extra_equations', allowed =
      join_cse envs_with_levels new_cse ~allowed
    in
    let need_other_round =
      (* If we introduce new parameters, then CSE equations involving the
         corresponding arguments can be considered again, so we need
         another round. *)
      not (Continuation_extra_params_and_args.is_empty extra_params')
    in
    let cse = EP.Map.disjoint_union prev_cse cse' in
    let extra_params =
      Continuation_extra_params_and_args.concat extra_params' extra_params
    in
    let extra_equations =
      Name.Map.disjoint_union extra_equations extra_equations'
    in
    cse, extra_params, extra_equations, allowed, need_other_round
  in
  let cse, extra_params, extra_equations, allowed =
    let rec do_rounds current_round cse extra_params extra_equations allowed =
      let cse, extra_params, extra_equations, allowed, need_other_round =
        compute_cse_one_round cse extra_params extra_equations allowed
      in
      if need_other_round && current_round < Flambda_features.cse_depth ()
      then begin
        do_rounds (succ current_round)
          cse extra_params extra_equations allowed
      end else begin
        (* Either a fixpoint has been reached or we've already explored far
           enough *)
        cse, extra_params, extra_equations, allowed
      end
    in
    do_rounds 1 EP.Map.empty Continuation_extra_params_and_args.empty
      Name.Map.empty allowed
  in
  let joined_types =
    Name.Map.union (fun name _ty_join _ty_cse ->
        Misc.fatal_errorf "Name %a from cse already present in joined_types"
          Name.print name)
      joined_types
      extra_equations
  in
  (*
  Format.eprintf "Joined CSE:@ %a\n%!"
    (Flambda_primitive.Eligible_for_cse.Map.print Simple.print) cse;
  Format.eprintf "allowed (final):@ %a\n%!" Name_occurrences.print allowed;
  *)
  (* Having calculated which equations to propagate, the resulting level can
     now be constructed. *)
  let t = construct_joined_level envs_with_levels ~allowed ~joined_types ~cse in
  (*
  Format.eprintf "Join result:@ %a\n%!" print t;
  *)
  t, extra_params

let n_way_join ~env_at_fork envs_with_levels ~params =
  match envs_with_levels with
  | [] -> empty (), Continuation_extra_params_and_args.empty
  | envs_with_levels -> join ~env_at_fork envs_with_levels ~params

let all_ids_for_export t =
  let variables = Variable.Map.keys t.defined_vars in
  let ids = Ids_for_export.create ~variables () in
  let equation name ty ids =
    let ids =
      Ids_for_export.union ids
        (Type_grammar.all_ids_for_export ty)
    in
    Ids_for_export.add_name ids name
  in
  let ids = Name.Map.fold equation t.equations ids in
  let cse prim simple ids =
    let ids, _ =
      Flambda_primitive.Eligible_for_cse.fold_args prim
        ~init:ids ~f:(fun ids simple ->
          Ids_for_export.add_simple ids simple, simple)
    in
    Ids_for_export.add_simple ids simple
  in
  let ids = Flambda_primitive.Eligible_for_cse.Map.fold cse t.cse ids in
  ids

let import _import_map _t =
  Misc.fatal_error "Import not implemented on Typing_env_level"
