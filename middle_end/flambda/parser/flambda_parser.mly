%{
open Fexpr

let make_loc (startpos, endpos) = Lambda.of_raw_location {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let make_tag ~loc:_ = function
  | s, None -> int_of_string s
  | _, Some _ ->
    failwith "No modifier allowed for tags"

let make_tagged_immediate ~loc:_ = function
  | s, Some 't' -> s
  | _, _ ->
    failwith "Tagged immediates must have modifier 't'"

let make_const_int (i, m) : Fexpr.const =
  match m with
  | None -> Naked_nativeint (Int64.of_string i)
  | Some 'u' -> Naked_immediate i
  | Some 't' -> Tagged_immediate i
  | Some 'l' -> Naked_int32 (Int32.of_string i)
  | Some 'L' -> Naked_int64 (Int64.of_string i)
  | Some c -> failwith (Printf.sprintf "Unknown int modifier %c" c)

let make_const_float (i, m) =
  match m with
  | None -> Naked_float (float_of_string i)
  | Some c -> failwith (Printf.sprintf "Unknown float modifier %c" c)
%}

/* Tokens */

%token AND   [@symbol "and"]
%token APPLY [@symbol "apply"]
%token AROBASE [@symbol "@"]
%token BANG [@symbol "!"]
%token BLOCK [@symbol "Block"]
%token CCALL  [@symbol "ccall"]
%token CLOSURE  [@symbol "closure"]
%token CODE  [@symbol "code"]
%token COLON  [@symbol ":"]
%token COLONEQUAL  [@symbol ":="]
%token COMMA  [@symbol ","]
%token CONT  [@symbol "cont"]
%token DEF   [@symbol "def"]
%token DOT   [@symbol "."]
%token EFFECT [@symbol "effect"]
%token EQUAL [@symbol "="]
%token EXN   [@symbol "exn"]
%token <string * char option> FLOAT
%token GET_FIELD
%token HCF   [@symbol "HCF"]
%token IN    [@symbol "in"]
%token IS_INT  [@symbol "is_int"]
%token <string * char option> INT
%token LBRACE [@symbol "{"]
%token LBRACKET [@symbol "["]
%token LET    [@symbol "let"]
%token LETK   [@symbol "letk"]
%token <string> LIDENT
%token LPAREN [@symbol "("]
%token MINUS    [@symbol "-"]
%token MINUSDOT [@symbol "-."]
%token MINUSGREATER [@symbol "->"]
%token MUT    [@symbol "mut"]
%token OPAQUE [@symbol "opaque_identity"]
%token PLUS     [@symbol "+"]
%token PLUSDOT  [@symbol "+."]
%token RBRACE [@symbol "}"]
%token RBRACKET [@symbol "]"]
%token REC    [@symbol "rec"]
%token RPAREN [@symbol ")"]
%token ROOT   [@symbol "root"]
%token SEMICOLON [@symbol ";"]
%token STUB   [@symbol "stub"]
%token STAR   [@symbol "*"]
%token SWITCH [@symbol "switch"]
%token TAG    [@symbol "tag"]
%token <string> UIDENT
%token UNDERSCORE [@symbol "_"]
%token UNREACHABLE [@symbol "Unreachable"]
%token EOF

%start program
%type <Fexpr.program> program
%type <Fexpr.definition> definition
%type <Fexpr.static_structure> static_structure
%type <Fexpr.expr> expr
(* %type <Fexpr.name> name *)
%type <Fexpr.of_kind_value> of_kind_value
%%

program:
  | elts = program_body_elt* EOF       { elts }
;

program_body_elt:
  | EFFECT e = effect                     { Define_symbol (Nonrecursive, e) }
  | DEF recu = recursive
        exn_cont = option(exn_continuation)
        def = definition
    { let def =
        match def.computation with
        | None -> def
        | Some comput ->
          { def with computation =
            Some { comput with exception_cont = exn_cont } }
      in
      Define_symbol (recu, def) }
  | LET CODE let_code = let_code          { Let_code let_code }
  | ROOT s = symbol                       { Root s }
;

exn_continuation:
  | STAR cont = continuation { cont }

let_code:
  | name = func_sym params = typed_args
  MINUSGREATER ret_cont = continuation
  exn_cont = option(exn_continuation)
  ret_arity = return_arity EQUAL expr = expr
  { ({ name; params; ret_cont; ret_arity; exn_cont; expr } : let_code) }
;

recursive:
  | { Nonrecursive }
  | REC { Recursive }
;

tag_to_size:
  | tag = INT COLON size = INT {
  let (tag, _) = tag in
  let (size, _) = size in
  int_of_string tag, int_of_string size
}

tags_to_sizes:
  | { [] }
  | tag_to_size = tag_to_size { [ tag_to_size ] }
  | tag_to_size = tag_to_size COMMA tags_to_sizes = tags_to_sizes { tag_to_size :: tags_to_sizes }

switch_sort:
  | TAG LBRACKET tags_to_sizes = tags_to_sizes RBRACKET { Tag { tags_to_sizes } }
  | { Int }
  | IS_INT { Is_int }
;

unop:
  | OPAQUE { Opaque_identity }

infix_binop:
  | PLUS { Plus }
  | PLUSDOT { Plusdot }
  | MINUS { Minus }
  | MINUSDOT { Minusdot }
;

binop:
  | a = simple DOT LPAREN f = simple RPAREN
    { Binop (Block_load (Block Value, Immutable), a, f) }

named:
  | s = simple { Simple s }
  | u = unop a = simple { Prim (Unop (u, a)) }
  | a1 = simple b = infix_binop a2 = simple { Prim (Infix_binop (b, a1, a2)) }
  | b = binop { Prim b }
  | BLOCK t = tag LPAREN elts = simple* RPAREN { Prim (Block (t, Immutable, elts)) }
  | BANG v = variable { Read_mutable v }
  | v = variable COLONEQUAL s = simple { Assign { being_assigned = v; new_value = s } }
;

switch_case:
  | i = tag MINUSGREATER c = continuation { i,c }
;

switch:
  | option(SEMICOLON) { [] }
  | c = switch_case { [c] }
  | h = switch_case SEMICOLON t = switch { h :: t }
;
kind:
  | { None }
;
return_kinds:
  | { [] }
  | k = kind COMMA t = return_kinds { k :: t }
;
return_arity:
  | { None }
  | COLON k = return_kinds { Some k }
;
expr:
  | HCF { Invalid Halt_and_catch_fire }
  | UNREACHABLE { Invalid Treat_as_unreachable }
  | CONT c = continuation s = simple_args { Apply_cont (c, None, s) }
  | SWITCH sort = switch_sort scrutinee = simple LBRACE cases = switch RBRACE
    { Switch {scrutinee; sort; cases} }
  | LET v = kinded_variable_opt EQUAL defining_expr = named IN body = expr
      { let (var, kind) = v in
        Let { var; kind; defining_expr; body } }
  | defining_expr = named SEMICOLON body = expr
      { Let { var = None; kind = None; defining_expr; body } }
  | LET MUT v = kinded_variable EQUAL initial_value = simple IN body = expr
      { let (var, kind) = v in
        Let_mutable { var; kind; initial_value; body } }
  | LETK recursive = recursive handler = continuation_handler t = andk body = expr
     { let handlers = handler :: t in
       Let_cont { recursive; body; handlers } }
  | CCALL LBRACKET func = csymbol RBRACKET args = simple_args ra = return_arity
    MINUSGREATER r = continuation e = exn_continuation
     { Apply {
          func = Symbol func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind = C_call {
              alloc = true; (* TODO noalloc *)
              (* param_arity =; *)
              return_arity = ra;
            };
       }}
  | APPLY func = name args = simple_args MINUSGREATER
    r = continuation e = exn_continuation
     { Apply {
          func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind = Function Indirect_unknown_arity;
       }}
  | CLOSURE r = recursive c = separated_nonempty_list(AND, closure) body = expr
    { Let_closure { closures = c; body; recursive = r } }
;

closure:
  | name = variable params = typed_args closure_vars = closure_elements
    MINUSGREATER ret_cont = continuation
    exn_cont = option(exn_continuation)
    ret_arity = return_arity LBRACE expr = expr RBRACE
    { { name; params; closure_vars; ret_cont; exn_cont; ret_arity; expr } }

exn_and_stub:
  | { false, false }
  | STUB { false, true }
  | EXN { true, false }
  | STUB EXN { true, true }
  | EXN STUB { true, true }
;

continuation_handler:
  | exn_and_stub = exn_and_stub name = continuation
    params = typed_args LBRACE handler = expr RBRACE
    { let is_exn_handler, stub = exn_and_stub in
      { name; params; stub; is_exn_handler; handler } }
;

andk:
  | AND h = continuation_handler t = andk { h :: t }
  | { [] }

definition:
  | static = static_structure+
      { { computation = None; static_structure = static } }
  | LETK c = continuation v = args LBRACE
         static = static_structure+
      RBRACE
      expr = expr
      { let computation =
          { expr; return_cont = c;
            exception_cont = None; computed_values = v }
        in
        { computation = Some computation; static_structure = static } }
;

effect:
  | c = continuation
    exn_cont = option(exn_continuation)
    v = args expr = expr
      { let computation =
          { expr; return_cont = c;
            exception_cont = exn_cont; computed_values = v }
        in
        { computation = Some computation; static_structure = [] } }
;

closure_elements:
  | LBRACKET v = variable* RBRACKET { v }
  | { [] }

typed_args:
  | LPAREN v = typed_variable* RPAREN { v }
  | { [] }

args:
  | LPAREN v = kinded_variable* RPAREN { v }
  | { [] }

static_structure:
  | s = symbol EQUAL BLOCK t = tag LPAREN elts = of_kind_value* RPAREN
    { ( s, None, Block (t, Immutable, elts) ) }
;

tag:
  tag = INT { make_tag ~loc:(make_loc ($startpos, $endpos)) tag }
;

of_kind_value:
  | s = symbol { Symbol s }
  | v = variable { Dynamically_computed v }
  | i = INT { Tagged_immediate ( make_tagged_immediate ~loc:($startpos, $endpos) i ) }
;

typed_variable:
  | param = variable { { param; ty = () } }
;

kinded_variable:
  | v = variable { v, None }
;

kinded_variable_opt:
  | v = variable_opt { v, None }
;

simple_args:
  | { [] }
  | LPAREN s = simple* RPAREN { s }
;

const:
  | c = INT { make_const_int c }
  | c = FLOAT { make_const_float c }
;

name:
  | s = symbol { (Symbol s:name) }
  | v = variable { (Var v:name) }
;

simple:
  | s = symbol { Symbol s }
  | v = variable { Var v }
  | c = const { Const c }
;

func_sym:
  | s = symbol { s }
;

symbol:
  | e = UIDENT { e, make_loc ($startpos, $endpos) }
;

csymbol:
  | s = LIDENT { s, make_loc ($startpos, $endpos) }
;

variable:
  | e = LIDENT { e, make_loc ($startpos, $endpos) }
;

variable_opt:
  | UNDERSCORE { None }
  | e = LIDENT { Some (e, make_loc ($startpos, $endpos)) }
;

continuation:
  | e = LIDENT { e, make_loc ($startpos, $endpos) }
;
%%
