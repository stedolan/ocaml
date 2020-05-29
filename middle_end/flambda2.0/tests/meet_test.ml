open! Flambda2

module K = Flambda_kind
module T = Flambda_type
module TE = T.Typing_env
module TEE = T.Typing_env_extension

let resolver _ = None

let test_meet_chains_two_vars () =
  let env = TE.create ~resolver in
  let var1 = Variable.create "var1" in
  let var1' = Var_in_binding_pos.create var1 Name_mode.normal in
  let env =
    TE.add_definition env (Name_in_binding_pos.var var1') K.value
  in
  let env =
    TE.add_equation env (Name.var var1)
      (T.immutable_block Tag.zero ~field_kind:K.value
        ~fields:[T.any_tagged_immediate ()])
  in
  let var2 = Variable.create "var2" in
  let var2' = Var_in_binding_pos.create var2 Name_mode.normal in
  let env =
    TE.add_definition env (Name_in_binding_pos.var var2') K.value
  in
  let first_type_for_var2 = T.alias_type_of K.value (Simple.var var1) in
  let env = TE.add_equation env (Name.var var2) first_type_for_var2 in
  let symbol =
    Symbol.create (Compilation_unit.get_current_exn ())
      (Linkage_name.create "my_symbol")
  in
  let env =
    TE.add_definition env (Name_in_binding_pos.symbol symbol) K.value
  in
  Format.eprintf "Initial situation:@ %a\n%!" TE.print env;
  let new_type_for_var2 = T.alias_type_of K.value (Simple.symbol symbol) in
  Format.eprintf "New knowledge:@ %a : %a\n%!"
    Variable.print var2
    T.print new_type_for_var2;
  match T.meet env first_type_for_var2 new_type_for_var2 with
  | Bottom -> assert false
  | Ok (meet_ty, env_extension) ->
    Format.eprintf "Env extension:@ %a\n%!" TEE.print env_extension;
    let env = TE.add_env_extension env env_extension in
    let env = TE.add_equation env (Name.var var2) meet_ty in
    Format.eprintf "Final situation:@ %a\n%!" TE.print env

let test_meet_chains_three_vars () =
  let env = TE.create ~resolver in
  let var1 = Variable.create "var1" in
  let var1' = Var_in_binding_pos.create var1 Name_mode.normal in
  let env =
    TE.add_definition env (Name_in_binding_pos.var var1') K.value
  in
  let env =
    TE.add_equation env (Name.var var1)
      (T.immutable_block Tag.zero ~field_kind:K.value
        ~fields:[T.any_tagged_immediate ()])
  in
  let var2 = Variable.create "var2" in
  let var2' = Var_in_binding_pos.create var2 Name_mode.normal in
  let env =
    TE.add_definition env (Name_in_binding_pos.var var2') K.value
  in
  let first_type_for_var2 = T.alias_type_of K.value (Simple.var var1) in
  let env = TE.add_equation env (Name.var var2) first_type_for_var2 in
  let var3 = Variable.create "var3" in
  let var3' = Var_in_binding_pos.create var3 Name_mode.normal in
  let env =
    TE.add_definition env (Name_in_binding_pos.var var3') K.value
  in
  let first_type_for_var3 = T.alias_type_of K.value (Simple.var var2) in
  let env = TE.add_equation env (Name.var var3) first_type_for_var3 in
  let symbol =
    Symbol.create (Compilation_unit.get_current_exn ())
      (Linkage_name.create "my_symbol")
  in
  let env =
    TE.add_definition env (Name_in_binding_pos.symbol symbol) K.value
  in
  Format.eprintf "Initial situation:@ %a\n%!" TE.print env;
  let new_type_for_var3 = T.alias_type_of K.value (Simple.symbol symbol) in
  Format.eprintf "New knowledge:@ %a : %a\n%!"
    Variable.print var3
    T.print new_type_for_var3;
  match T.meet env first_type_for_var3 new_type_for_var3 with
  | Bottom -> assert false
  | Ok (meet_ty, env_extension) ->
    Format.eprintf "Env extension:@ %a\n%!" TEE.print env_extension;
    let env = TE.add_env_extension env env_extension in
    let env = TE.add_equation env (Name.var var3) meet_ty in
    Format.eprintf "Final situation:@ %a\n%!" TE.print env

let () =
  let comp_unit =
    Compilation_unit.create (Ident.create_persistent "Meet_test")
      (Linkage_name.create "meet_test")
  in
  Compilation_unit.set_current comp_unit;
  Format.eprintf "MEET CHAINS WITH TWO VARS\n\n%!";
  test_meet_chains_two_vars ();
  Format.eprintf "\nMEET CHAINS WITH THREE VARS\n\n%!";
  test_meet_chains_three_vars ();
