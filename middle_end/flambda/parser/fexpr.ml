[@@@ocaml.warning "-30"]

type location = Lambda.scoped_location

type closure_id = string * location

type symbol = string * location
type variable = string * location
type variable_opt = (string * location) option
type continuation = string * location
type func_sym = symbol

type immediate = string
type targetint = int64

type const =
  | Naked_immediate of immediate
  | Tagged_immediate of immediate
  | Naked_float of float
  | Naked_int32 of Int32.t
  | Naked_int64 of Int64.t
  | Naked_nativeint of targetint

type of_kind_value =
  | Symbol of symbol
  | Tagged_immediate of immediate
  | Dynamically_computed of variable

type is_recursive =
  | Nonrecursive
  | Recursive

type tag_scannable = int

type static_part =
  | Block of tag_scannable * Mutability.t * of_kind_value list

module Naked_number_kind = struct
  type t =
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint
end

type kind =
  | Value
  | Naked_number of Naked_number_kind.t
  | Fabricated

type okind = kind option
type flambda_type = unit

type static_structure = (symbol * okind * static_part)

type invalid_term_semantics =
  | Treat_as_unreachable
  | Halt_and_catch_fire

type trap_action
type typed_parameter = {
  param : variable;
  ty : flambda_type;
}

type name =
  | Var of variable
  | Symbol of symbol

type simple =
  | Var of variable
  | Symbol of symbol
  | Const of const

type unop =
  | Opaque_identity

type generic_array_specialisation =
  | No_specialisation
  | Full_of_naked_floats
  | Full_of_immediates
  | Full_of_arbitrary_values_but_not_floats

type block_access_kind =
  | Block of kind
  | Array of kind
  | Generic_array of generic_array_specialisation

type binop =
  | Block_load of block_access_kind * Mutability.t

type infix_binop =
  | Plus | Plusdot
  | Minus | Minusdot

type prim =
  | Unop of unop * simple
  | Infix_binop of infix_binop * simple * simple
  | Binop of binop * simple * simple
  | Block of tag_scannable * Mutability.t * simple list

type named =
  | Simple of simple
  | Prim of prim
  (* | Set_of_closures of Set_of_closures.t *)
  | Assign of {
      being_assigned : variable;
      new_value : simple;
    }
  | Read_mutable of variable

type is_fabricated =
  | Value | Fabricated

type flambda_arity = okind list

type function_call =
  | Direct of {
      closure_id : closure_id;
      return_arity : flambda_arity;
    }
  | Indirect_unknown_arity
  | Indirect_known_arity of {
      param_arity : flambda_arity;
      return_arity : flambda_arity;
    }

type method_kind = Self | Public | Cached

type call_kind =
  | Function of function_call
  | Method of { kind : method_kind; obj : name; }
  | C_call of {
      alloc : bool;
      (* param_arity : flambda_arity; To recover from args *)
      return_arity : flambda_arity option;
    }

type apply = {
    func : name;
    continuation : continuation;
    exn_continuation : continuation;
    args : simple list;
    call_kind : call_kind;
    (* dbg : Debuginfo.t; *)
    (* inline : inline_attribute;
     * specialise : specialise_attribute; *)
  }

type size = int

type switch_sort =
  | Int
  | Tag of { tags_to_sizes : (tag_scannable * size) list; }
  | Is_int

type expr =
  | Let of let_
  | Let_closure of let_closure
  | Let_mutable of {
      var : variable;
      initial_value : simple;
      kind : okind;
      body : expr;
    }
  | Let_cont of let_cont
  | Apply of apply
  | Apply_cont of continuation * trap_action option * simple list
  | Switch of {
      scrutinee : simple;
      sort : switch_sort;
      cases : (int * continuation) list;
    }
  | Invalid of invalid_term_semantics

and closure = {
  name : variable;
  params : typed_parameter list;
  closure_vars : variable list;
  ret_cont : continuation;
  exn_cont : continuation option;
  ret_arity : flambda_arity option;
  expr : expr;
}

and let_closure = {
  recursive : is_recursive;
  closures : closure list;
  body : expr;
}

and let_ = {
    var : variable_opt;
    kind : okind;
    defining_expr : named;
    body : expr;
  }

and let_cont = {
  recursive : is_recursive;
  body : expr;
  handlers : let_cont_handlers;
}

and let_cont_handlers = continuation_handler list

and continuation_handler = {
  name : continuation;
  params : typed_parameter list;
  stub : bool;
  is_exn_handler : bool;
  handler : expr;
}

type computation = {
  expr : expr;
  return_cont : continuation;
  exception_cont : continuation option;
  computed_values : (variable * okind) list;
}

type definition = {
  computation : computation option;
  static_structure : static_structure list;
}

type let_code = {
  name : func_sym;
  params : typed_parameter list;
  ret_cont : continuation;
  exn_cont : continuation option;
  ret_arity : flambda_arity option;
  expr : expr;
}

type program_body_elt =
  | Root of symbol
  | Let_code of let_code
  | Define_symbol of is_recursive * definition

type program = program_body_elt list
