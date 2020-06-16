[@@@ocaml.warning "+a-4-30-40-41-42"]

module C = struct
  type t = Fexpr.continuation
  let print ppf ((_, loc) as c) =
    Format.fprintf ppf
      "%a : %a"
      Print_fexpr.continuation c
      Lambda.print_scoped_location loc
  let compare ((c1, _):t) ((c2, _):t) =
    String.compare c1 c2
end
module CM = Map.Make(C)

module V = struct
  type t = Fexpr.variable
  let compare ((c1, _):t) ((c2, _):t) =
    String.compare c1 c2
end
module VM = Map.Make(V)

module Func_sym = struct
  type t = Fexpr.func_sym
  let compare ((c1, _):t) ((c2, _):t) =
    String.compare c1 c2
end
module FM = Map.Make(Func_sym)

let conv_rec : Fexpr.is_recursive -> Asttypes.rec_flag = function
  | Nonrecursive -> Nonrecursive
  | Recursive -> Recursive

type env = {
  functions : Ilambda.function_declaration FM.t;
  continuations : (Continuation.t * int) CM.t;
  variables : Ident.t VM.t;
}

let init_env functions= {
  functions;
  continuations = CM.empty;
  variables = VM.empty;
}

let exn_continuation c : Ilambda.exn_continuation =
  { exn_handler = c;
    extra_args = []; (* TODO *) }

let fresh_cont env ?sort c arity =
  let c' = Continuation.create ?sort () in
  c',
  { env with
    continuations = CM.add c (c', arity) env.continuations }

let fresh_var env ((name, _loc) as v) =
  let v' = Ident.create_local name in
  v',
  { env with
    variables = VM.add v v' env.variables }

let simple_ident env (s:Fexpr.simple) : Ilambda.simple =
  match s with
  | Var v ->
      Var (VM.find v env.variables)
  | _ ->
      (* TODO: decompose *)
      Misc.fatal_error "not an ident"

let simple_idents env s =
  List.map (simple_ident env) s

let infix_binop (op:Fexpr.infix_binop) : Lambda.primitive =
  match op with
  | Plus -> Paddint
  | Minus -> Psubint
  | _ -> failwith "TODO prim"

let defining_expr env (named:Fexpr.named) : Ilambda.named =
  match named with
  | Simple (Var v) ->
      Simple (Var (VM.find v env.variables))
  | Simple (Const (Tagged_immediate i)) ->
      let i = int_of_string i in
      Simple (Const (Const_base (Const_int i)))
  | Prim (Infix_binop (op, a1, a2)) ->
      Prim {
        prim = infix_binop op;
        args = simple_idents env [a1; a2];
        loc = Loc_unknown;
        exn_continuation = None;
      }
  | Prim (Block (tag, mut, args)) ->
      let mut : Lambda.mutable_flag =
        match mut with
        | Mutable -> Mutable
        | Immutable -> Immutable
        | Immutable_unique -> Immutable_unique
      in
      Prim {
        prim = Pmakeblock (tag, mut, None);
        args = simple_idents env args;
        loc = Loc_unknown;
        exn_continuation = None;
      }
  | _ ->
      failwith "defining expr"

let value_kind (kind : Fexpr.okind) : Lambda.value_kind =
  match kind with
  | None | Some Value ->
    Pgenval
  | Some (Naked_number _ | Fabricated) ->
    Misc.fatal_error "Forbiden kind"

let value_type_kind (() : Fexpr.flambda_type) : Lambda.value_kind =
  Pgenval

let rec expr (env:env) (e : Fexpr.expr) : Ilambda.t =
  match e with
  | Apply_cont (cont, None, args) ->
      let c, arity = CM.find cont env.continuations in
      if List.length args <> arity then
        Misc.fatal_errorf "wrong continuation arity %a" C.print cont;
      let args = simple_idents env args in
      Apply_cont (c, None, args)
  | Let { var = Some var; kind; defining_expr = d; body } ->
      let id, env = fresh_var env var in
      let named = defining_expr env d in
      let body = expr env body in
      Let (id, User_visible, value_kind kind, named, body)
  | Let_cont
      { recursive; body;
        handlers = [handler] } ->
      let name, body_env =
        fresh_cont env handler.name (List.length handler.params)
      in
      let env =
        match recursive with
        | Nonrecursive -> env
        | Recursive -> body_env
      in
      let body = expr body_env body in
      let handler_env, params =
        List.fold_right
          (fun ({ param; ty }:Fexpr.typed_parameter)
            (env, args) ->
            let var, env = fresh_var env param in
            env, (var, Ilambda.User_visible, value_type_kind ty) :: args)
          handler.params (env, [])
      in
      let handler =
        expr handler_env handler.handler
      in
      Let_cont
        { name;
          is_exn_handler = false;
          params;
          recursive = conv_rec recursive;
          body;
          handler; }
  | _ ->
      failwith "TODO expr"

(* let computation (c : Fexpr.computation) : Ilambda.program =
 *   let cont_arity = List.length c.computed_values in
 *   let return_continuation, env = fresh_cont init_env c.return_cont cont_arity in
 *   let exn_cont, env = fresh_cont env c.exception_cont 1 in
 *   let expr = expr env c.expr in
 *   { expr;
 *     return_continuation;
 *     exn_continuation = exn_continuation exn_cont;
 *     uses_mutable_variables = false } *)

let of_kind_value_var : Fexpr.of_kind_value -> _ = function
  | Dynamically_computed v -> v
  | _ -> assert false

type program = {
  program : Ilambda.program;
  module_ident : Ident.t;
  size : int;
}

let rec conv_top func_env (prog : Fexpr.program) : program =
  match prog with
  | [] -> assert false
  | Let_code let_code :: prog_body ->
      let env = init_env func_env in
      let return_continuation, env = fresh_cont ~sort:Return env let_code.ret_cont 1 in
      let exn_cont, env =
        let cont = match let_code.exn_cont with
          | None -> "*dummy*", Lambda.Loc_unknown
          | Some c -> c
        in
        fresh_cont ~sort:Exn env cont 1
      in
      let env, params =
        List.fold_right (fun (arg : Fexpr.typed_parameter) (env, params) ->
            let var, env = fresh_var env arg.param in
            env, (var, value_type_kind arg.ty) :: params)
          let_code.params (env, [])
      in
      let body = expr env let_code.expr in
      let declaration : Ilambda.function_declaration = {
        kind = Curried;
        return_continuation;
        exn_continuation = exn_continuation exn_cont;
        params;
        return = Pgenval;
        body;
        free_idents_of_body = Ident.Set.empty;
        attr = Lambda.default_function_attribute;
        loc = Loc_unknown;
        stub = false;
      } in
      let func_env = FM.add let_code.name declaration func_env in
      conv_top func_env prog_body
  | Define_symbol
      (Nonrecursive,
       { computation = Some c;
         static_structure =
           [ _symbol, _kind,
             Block (tag, Immutable, elts) ] }) :: [Root __symbol] ->
      let cont_arity = List.length c.computed_values in
      let init_env = init_env func_env in
      let return_continuation', env = fresh_cont init_env c.return_cont cont_arity in
      let return_continuation = Continuation.create () in
      let exn_cont, env =
        match c.exception_cont with
        | None ->
          Continuation.create ~sort:Exn (), env
        | Some exc_cont ->
          fresh_cont ~sort:Exn env exc_cont 1
      in
      let main_expr = expr env c.expr in
      let params, penv =
        List.fold_right (fun (var, kind) (params, env) ->
            let var, env = fresh_var env var in
            (var, Ilambda.Not_user_visible, value_kind kind) :: params, env)
          c.computed_values ([], init_env)
      in
      let handler : Ilambda.t =
        let id = Ident.create_local "return" in
        Let (id, Not_user_visible, Pgenval,
             Prim {
               prim = Pmakeblock (tag, Immutable, None);
               args =
                 List.map (fun v : Ilambda.simple ->
                     Var (VM.find (of_kind_value_var v) penv.variables))
                   elts;
               loc = Loc_unknown;
               exn_continuation = None;
             },
             Apply_cont (return_continuation, None, [Ilambda.Var id]))
      in
      let expr : Ilambda.t =
        Let_cont {
          name = return_continuation';
          is_exn_handler = false;
          params;
          recursive = Nonrecursive;
          body = main_expr;
          handler; (* makeblock *)
        }
      in
      let program : Ilambda.program =
        { expr;
          return_continuation;
          exn_continuation = exn_continuation exn_cont;
          uses_mutable_variables = false }
      in
      { program;
        module_ident = Ident.create_local "module";
        size = List.length elts }
  | _ ->
      assert false

let conv p = conv_top FM.empty p
