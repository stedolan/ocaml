(* Needs fixing
module Program_body = Flambda_static.Program_body
module Named = Flambda.Named
module E = Flambda.Expr
module I = Flambda_kind.Standard_int
module P = Flambda_primitive
module K = Flambda_kind
module KP = Kinded_parameter

module C = struct
  type t = string
  let print ppf c = Format.pp_print_string ppf c
  let compare = String.compare
end
module CM = Map.Make(C)

module V = struct
  type t = string
  let print ppf c = Format.pp_print_string ppf c
  let compare = String.compare
end
module VM = Map.Make(V)

module S = struct
  type t = string
  let print ppf c = Format.pp_print_string ppf c
  let compare = String.compare
end
module SM = Map.Make(S)

type func_env = {
  code : Fexpr.let_code SM.t;
  symbols : Symbol.t SM.t;
}

let init_fenv = {
  code = SM.empty;
  symbols = SM.empty;
}

type env = {
  continuations : (Continuation.t * int) CM.t;
  exn_continuations : Exn_continuation.t CM.t;
  variables : Variable.t VM.t;
  symbols : Symbol.t SM.t;
}

let init_env (func_env:func_env) = {
  continuations = CM.empty;
  exn_continuations = CM.empty;
  variables = VM.empty;
  symbols = func_env.symbols ;
}

let enter_closure env = {
  continuations = CM.empty;
  exn_continuations = CM.empty;
  variables = VM.empty;
  symbols = env.symbols;
}

let fresh_cont env ?sort (c, _loc) arity =
  let c' = Continuation.create ?sort () in
  c',
  { env with
    continuations = CM.add c (c', arity) env.continuations }

let fresh_exn_cont env (c, _loc) =
  let c' = Continuation.create ~sort:Exn () in
  let e = Exn_continuation.create ~exn_handler:c' ~extra_args:[] in
  c', e,
  { env with
    exn_continuations = CM.add c e env.exn_continuations }

let fresh_var env (name, _loc) =
  let v = Variable.create name in
  v,
  { env with
    variables = VM.add name v env.variables }

let declare_symbol ~backend:_ (env:func_env) (name, loc) =
  if SM.mem name env.symbols then
    Misc.fatal_errorf "Redefinition of symbol %s: %a"
      name Location.print_loc loc
  else
    (* let module Backend = (val backend : Flambda_backend_intf.S) in
     * let symbol = Backend.symbol_for_global' (Ident.create_persistent name) in *)
    let symbol =
      Symbol.create
        (Compilation_unit.get_current_exn ())
        (Linkage_name.create name)
    in
    symbol,
    { env with
      symbols = SM.add name symbol env.symbols }

let get_symbol (env:env) (name, loc) =
  match SM.find_opt name env.symbols with
  | None ->
    Misc.fatal_errorf "Unbound symbol %s: %a"
      name Location.print_loc loc
  | Some s ->
    s

let find_cont env (c, loc) =
  match CM.find_opt c env.continuations with
  | None ->
    Misc.fatal_errorf "Unbound continuation %s: %a"
      c Location.print_loc loc
  | Some c ->
    c

let find_exn_cont env (c, loc) =
  match CM.find_opt c env.exn_continuations with
  | None ->
    Misc.fatal_errorf "Unbound exn_continuation %s: %a"
      c Location.print_loc loc
  | Some c ->
    c

let find_var env (v, loc) =
  match VM.find_opt v env.variables with
  | None ->
    Misc.fatal_errorf "Unbound variable %s : %a" v
      Location.print_loc loc
  | Some var ->
    var

let const (c:Fexpr.const) : Reg_width_const.t =
  match c with
  | Tagged_immediate i ->
    let i = Targetint.of_string i in
    Tagged_immediate (Target_imm.int (Targetint.OCaml.of_targetint i))
  | Naked_immediate i ->
    let i = Targetint.of_string i in
    Naked_immediate (Target_imm.int (Targetint.OCaml.of_targetint i))

  (*
   * | Naked_float of Numbers.Float_by_bit_pattern.t
   * | Naked_int32 of Int32.t
   * | Naked_int64 of Int64.t
   * | Naked_nativeint of Targetint.t *)
  | _ ->
    failwith "TODO const"

let simple env (s:Fexpr.simple) : Simple.t =
  match s with
  | Var (v, loc) -> begin
      match VM.find_opt v env.variables with
      | None ->
        Misc.fatal_errorf "Unbound variable %s : %a" v
          Location.print_loc loc
      | Some var ->
        Simple.var var
    end
  | Const c ->
    Simple.const (const c)
  | Symbol sym -> begin
      Simple.symbol (get_symbol env sym)
    end

let name env (s:Fexpr.name) : Simple.t =
  match s with
  | Var (v, loc) -> begin
      match VM.find_opt v env.variables with
      | None ->
        Misc.fatal_errorf "Unbound variable %s : %a" v
          Location.print_loc loc
      | Some var ->
        Simple.var var
    end
  | Symbol sym -> begin
      Simple.symbol (get_symbol env sym)
    end

let of_kind_value env (v:Fexpr.of_kind_value) : Flambda_static.Of_kind_value.t =
  match v with
  | Symbol s ->
    Symbol (get_symbol env s)
  | Tagged_immediate i ->
    let i = Targetint.of_string i in
    Tagged_immediate
      (Target_imm.int (Targetint.OCaml.of_targetint i))
  | Dynamically_computed var ->
    let var = find_var env var in
    Dynamically_computed var

let unop (unop:Fexpr.unop) : Flambda_primitive.unary_primitive =
  match unop with
  | Opaque_identity -> Opaque_identity

let infix_binop (binop:Fexpr.infix_binop) : Flambda_primitive.binary_primitive =
  match binop with
  | Plus -> Int_arith (I.Tagged_immediate, Add)
  | Minus -> Int_arith (I.Tagged_immediate, Sub)
  | Plusdot
  | Minusdot -> failwith "TODO binop"

let binop (binop:Fexpr.binop) : Flambda_primitive.binary_primitive =
  match binop with
  | Block_load (Block Value, Immutable) ->
    Block_load (Block (Value (Anything)), Immutable)
  | Block_load (_, _) ->
    failwith "TODO"

let convert_mutable_flag (flag : Fexpr.mutable_or_immutable)
      : Effects.mutable_or_immutable =
  match flag with
  | Mutable -> Mutable
  | Immutable -> Immutable

let convert_static_mutable_flag (flag : Fexpr.mutable_or_immutable)
      : Flambda_static.Static_part.mutable_or_immutable =
  match flag with
  | Mutable -> Mutable
  | Immutable -> Immutable

let convert_recursive_flag (flag : Fexpr.is_recursive) : Recursive.t =
  match flag with
  | Recursive -> Recursive
  | Nonrecursive -> Non_recursive

let convert_block_shape ~num_fields =
  List.init num_fields (fun _field : P.Value_kind.t -> Anything)

let defining_expr env (named:Fexpr.named) : Named.t =
  match named with
  | Simple s ->
    Named.create_simple (simple env s)
  | Prim (Unop (u, arg)) ->
    let prim : Flambda_primitive.t =
      Unary (unop u, simple env arg)
    in
    Named.create_prim prim Debuginfo.none
  | Prim (Infix_binop (b, a1, a2)) ->
    let prim : Flambda_primitive.t =
      Binary (infix_binop b, simple env a1, simple env a2)
    in
    Named.create_prim prim Debuginfo.none
  | Prim (Binop (b, a1, a2)) ->
    let prim : Flambda_primitive.t =
      Binary (binop b, simple env a1, simple env a2)
    in
    Named.create_prim prim Debuginfo.none
  | Prim (Block (tag, mutability, args)) ->
    let mutability = convert_mutable_flag mutability in
    let shape = convert_block_shape ~num_fields:(List.length args) in
    let kind : P.make_block_kind =
      Full_of_values (Tag.Scannable.create_exn tag, shape)
    in
    let prim : P.t =
      P.Variadic (
        Make_block (kind, mutability),
        List.map (simple env) args
      )
    in
    Named.create_prim prim Debuginfo.none
  | _ -> assert false

let value_kind _ = Flambda_kind.value

let rec expr env (e : Fexpr.expr) : E.t =
  match e with
  | Let { var = Some var; kind = _; defining_expr = d; body } ->
    let named = defining_expr env d in
    let id, env = fresh_var env var in
    let body = expr env body in
    let var =
      Var_in_binding_pos.create id Name_mode.normal
    in
    E.create_let var named body
  | Let_cont
      { recursive; body;
        handlers = [handler] } -> begin
      let is_exn_handler = false in
      let name, body_env =
        fresh_cont env handler.name (List.length handler.params)
      in
      let body = expr body_env body in
      let env =
        match recursive with
        | Nonrecursive -> env
        | Recursive -> body_env
      in
      let handler_env, params =
        List.fold_right
          (fun ({ param; ty }:Fexpr.typed_parameter)
            (env, args) ->
            let var, env = fresh_var env param in
            let user_visible = true in
            let param = Variable.with_user_visible var ~user_visible in
            let param = Kinded_parameter.create (Parameter.wrap param) (value_kind ty) in
            env, param :: args)
          handler.params (env, [])
      in
      let handler =
        expr handler_env handler.handler
      in
      let params_and_handler =
        Flambda.Continuation_params_and_handler.create params ~handler
      in
      let handler =
        Flambda.Continuation_handler.create ~params_and_handler
          ~stub:false
          ~is_exn_handler:is_exn_handler
      in
      match recursive with
      | Nonrecursive ->
        Flambda.Let_cont.create_non_recursive name handler ~body
      | Recursive ->
        let handlers = Continuation.Map.singleton name handler in
        Flambda.Let_cont.create_recursive handlers ~body
    end

  | Apply_cont ((cont, _loc) as cont', None, args) ->
    let c, arity = find_cont env cont' in
    if List.length args <> arity then
      Misc.fatal_errorf "wrong continuation arity %a" C.print cont;
    let args = List.map (simple env) args in
    let apply_cont = Flambda.Apply_cont.create c ~args ~dbg:Debuginfo.none in
    E.create_apply_cont apply_cont

  | Switch { scrutinee; cases } ->
    let arms =
      Target_imm.Map.of_list
        (List.map (fun (case, (arm, _loc)) ->
           let c, arity = CM.find arm env.continuations in
           assert(arity = 0);
           Target_imm.int (Targetint.OCaml.of_int case), c)
           cases)
    in
    Flambda.Expr.create_switch
      ~scrutinee:(simple env scrutinee)
      ~arms

  | Let_closure { recursive; closures; body } ->

    let fun_decl (function_declarations, closure_elements, env, closure_vars)
          ({ name;
             params;
             closure_vars = _;
             ret_cont;
             exn_cont;
             ret_arity;
             expr = closure_expr; } : Fexpr.closure) =

      let closure_env = enter_closure env in
      let my_closure, closure_env = fresh_var closure_env name in
      let recursive = convert_recursive_flag recursive in
      let compilation_unit = Compilation_unit.get_current_exn () in
      let closure_id = Closure_id.wrap compilation_unit my_closure in
      let arity =
        match ret_arity with
        | None -> 1
        | Some l -> List.length l
      in

      (* let closure_env, closure_elements =
       *   List.fold_left (fun (closure_env, closure_elements) closure_var ->
       *     let var_within_closure = Var_within_closure.wrap (Variable.create name) in
       *     let external_var = Simple.var (find_var env closure_var) in
       *     closure_env,
       *     Var_within_closure.Map.add var_within_closure external_var map)
       *     (closure_env, closure_elements)
       *     closure_vars
       * in *)

      let _ = closure_elements in
      let closure_elements = Var_within_closure.Map.empty in (*TODO*)

      let return_continuation, closure_env =
        fresh_cont closure_env ret_cont arity
      in
      let exn_handler, closure_env =
        match exn_cont with
        | None ->
          (* Not bound *)
          Continuation.create ~sort:Exn (), closure_env
        | Some exn_cont ->
          fresh_cont ~sort:Exn closure_env exn_cont 1
      in
      let exn_continuation =
        Exn_continuation.create ~exn_handler ~extra_args:[]
      in
      let closure_env, params =
        List.fold_right
          (fun ({ param; ty }:Fexpr.typed_parameter)
            (env, args) ->
            let var, env = fresh_var env param in
            let user_visible = true in
            let param = Variable.with_user_visible var ~user_visible in
            let param = Kinded_parameter.create (Parameter.wrap param) (value_kind ty) in
            env, param :: args)
          params (closure_env, [])
      in
      let body = expr closure_env closure_expr in

      let _params_and_body =
        Flambda.Function_params_and_body.create
          ~return_continuation
          exn_continuation params ~body ~my_closure
      in
      let code_id =
        Code_id.create ~name:(Closure_id.to_string closure_id)
          (Compilation_unit.get_current_exn ())
      in
      let function_declaration =
        Flambda.Function_declaration.create
          ~code_id
          ~params_arity:(KP.List.arity params)
          ~result_arity:[K.value]
          ~stub:false
          ~dbg:Debuginfo.none
          ~inline:Default_inline
          ~is_a_functor:false
          ~recursive
      in
      let _ =
        Misc.fatal_error "This needs fixing to register [params_and_body] \
          in the environment as a piece of code with [code_id]"
      in
      let bound_closure_var, env = fresh_var env name in
      let closure_vars =
        Closure_id.Map.add closure_id
          (Var_in_binding_pos.create
             bound_closure_var
             Name_mode.normal)
          closure_vars
      in

      Closure_id.Map.add closure_id function_declaration function_declarations,
      closure_elements,
      env,
      closure_vars
    in

    let function_declarations, closure_elements, env, closure_vars =
      List.fold_left fun_decl
        (Closure_id.Map.empty, Var_within_closure.Map.empty, env, Closure_id.Map.empty)
        closures
    in
    let body = expr env body in
    let function_decls =
      Flambda.Function_declarations.create function_declarations
    in
    let set_of_closures =
      Flambda.Set_of_closures.create function_decls ~closure_elements
    in
    Flambda.Expr.create_pattern_let
      (Bindable_let_bound.set_of_closures ~closure_vars)
      (Flambda.Named.create_set_of_closures set_of_closures)
      body

  | Apply {
    func;
    call_kind = Function Indirect_unknown_arity;
    continuation;
    exn_continuation;
    args; } ->
    let call_kind =
      Call_kind.indirect_function_call_unknown_arity ()
    in
    let continuation, _arity = find_cont env continuation in
    let exn_continuation = find_exn_cont env exn_continuation in
    let apply =
      Flambda.Apply.create
        ~callee:(name env func)
        ~continuation
        exn_continuation
        ~args:((List.map (simple env)) args)
        ~call_kind
        (Debuginfo.none)
        ~inline:Default_inline
        ~inlining_depth:0
    in
    Flambda.Expr.create_apply apply

  | Apply _ ->
    failwith "TODO apply"

  | _ ->
    failwith "TODO"

let rec conv_top ~backend (func_env:func_env) (prog : Fexpr.program) : Program_body.t =
  match prog with
  | [] -> assert false
  | Root (_, _loc) :: _ :: _ ->
    Misc.fatal_errorf "Root must be the last construction of the file"
  | [ Root s ] ->
    (* let module Backend = (val backend : Flambda_backend_intf.S) in
     * let symbol = Backend.symbol_for_global' (Ident.create_persistent s) in *)
    let symbol = get_symbol (init_env func_env) s in
    Program_body.root symbol
  | Define_symbol
      (Nonrecursive,
       { computation = Some c;
         static_structure = [ ] }) :: tail ->
    let cont_arity = List.length c.computed_values in
    let env = init_env func_env in
    let return_continuation, env = fresh_cont env ~sort:Return c.return_cont cont_arity in
    let exn_handler, env =
      match c.exception_cont with
      | None ->
        Continuation.create ~sort:Exn (), env
      | Some exc ->
        fresh_cont ~sort:Exn env exc 1
    in
    let exn_continuation = Exn_continuation.create ~exn_handler ~extra_args:[] in
    let computation_expr = expr env c.expr in
    let computation : Program_body.Computation.t = {
      expr = computation_expr;
      return_continuation;
      exn_continuation;
      computed_values = [];
    } in
    let body = conv_top ~backend func_env tail in
    Program_body.define_symbol ~body
      { computation = Some computation;
        static_structure = [];
      }
      Code_age_relation.empty
  | Define_symbol
      (Nonrecursive,
       { computation;
         static_structure }) :: tail ->
    let prev_env, computation =
      match computation with
      | None -> init_env func_env, None
      | Some c ->
        let cont_arity = List.length c.computed_values in
        let env = init_env func_env in
        let return_continuation, env = fresh_cont env ~sort:Return c.return_cont cont_arity in
        let _exn_handler, exn_continuation, env =
          match c.exception_cont with
          | None ->
            let exn_handler = Continuation.create ~sort:Exn () in
            let exn_continuation = Exn_continuation.create ~exn_handler ~extra_args:[] in
            exn_handler, exn_continuation, env
          | Some exc ->
            fresh_exn_cont env exc
        in
        let computation_expr = expr env c.expr in
        let env, computed_values =
          List.fold_right (fun (var, _kind) (env, acc) ->
            let var, env = fresh_var env var in
            let comp =
              Kinded_parameter.create (Parameter.create (Name.var var)) Flambda_kind.value
            in
            env, comp :: acc)
            c.computed_values (env, [])
        in
        let computation : Program_body.Computation.t = {
          expr = computation_expr;
          return_continuation;
          exn_continuation;
          computed_values;
        } in
        env, Some computation
    in
    let conv_static_structure (func_env, acc) (symbol, _kind, (def:Fexpr.static_part)) =
      match def with
      | Block (tag, mutability, args) ->
        let symbol, func_env = declare_symbol ~backend func_env symbol in
        (* let env = init_env func_env in *)
        let mutability = convert_static_mutable_flag mutability in
        let static_structure =
          let tag = Tag.Scannable.create_exn tag in
          Flambda_static.Static_part.Block
            (tag, mutability,
             List.map (of_kind_value prev_env) args)
        in
        let def : Program_body.Static_structure.t0 =
          S (Program_body.Bound_symbols.Singleton symbol, static_structure)
        in
        func_env, def :: acc
      (* | _ ->
       *   assert false *)
    in
    let func_env, structure =
      List.fold_left conv_static_structure (func_env, []) static_structure
    in
    let structure = List.rev structure in
    let body = conv_top ~backend func_env tail in
    Program_body.define_symbol ~body
      { computation;
        static_structure = structure;
      }
      Code_age_relation.empty

  | Let_code code :: tail ->
    let (name, _loc) = code.name in
    let func_env = {
      func_env with code = SM.add name code func_env.code;
    } in
    conv_top ~backend func_env tail

  | _ ->
    assert false

let conv ~backend fexpr : Flambda_static.Program.t =
  let body = conv_top ~backend init_fenv fexpr in
  { imported_symbols = Symbol.Map.empty;
    body; }
*)
let conv ~backend:_ _fexpr = assert false
