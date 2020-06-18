
{
open Lexing
open Flambda_parser

type location = Lexing.position * Lexing.position

type error =
  | Illegal_character of char
  | Invalid_literal of string
;;

let pp_error ppf = function
  | Illegal_character c -> Format.fprintf ppf "Illegal character %c" c
  | Invalid_literal s -> Format.fprintf ppf "Invalid litral %s" s

exception Error of error * location;;

let current_location lexbuf =
  (Lexing.lexeme_start_p lexbuf,
   Lexing.lexeme_end_p lexbuf)

let update_loc lexbuf file ~line ~absolute ~chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

let create_hashtable init =
  let tbl = Hashtbl.create (List.length init) in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

let keyword_table =
  create_hashtable [
    "and", AND;
    "apply", APPLY;
    "ccall", CCALL;
    "closure", CLOSURE;
    "code", CODE;
    "cont", CONT;
    "def", DEF;
    "effect", EFFECT;
    "exn", EXN;
    "in", IN;
    "is_int", IS_INT;
    "let", LET;
    "letk", LETK;
    "mut", MUT;
    "rec", REC;
    "root", ROOT;
    "stub", STUB;
    "switch", SWITCH;
    "tag", TAG;
]

let ukeyword_table =
  create_hashtable [
    "Opaque", OPAQUE;
    "Block", BLOCK;
    "Get_field", GET_FIELD;
    "HCF", HCF;
    "Unreachable", UNREACHABLE;
]

}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let dotsymbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '/' ':' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let literal_modifier = ['G'-'Z' 'g'-'z']

rule token = parse
  | newline
      { update_loc lexbuf None ~line:1 ~absolute:false ~chars:0;
        token lexbuf }
  | blank +
      { token lexbuf }
  | "(*"
      { comment 1 lexbuf;
        token lexbuf }
  | "let"
      { LET }
  | "_" { UNDERSCORE }
  | "!"
      { BANG }
  | "@"
      { AROBASE }
  | ","
      { COMMA }
  | ":"
      { COLON }
  | ":="
      { COLONEQUAL }
  | "."
      { DOT }
  | ";"
      { SEMICOLON }
  | "="
      { EQUAL }
  | "{"
      { LBRACE }
  | "}"
      { RBRACE }
  | "["
      { LBRACKET }
  | "]"
      { RBRACKET }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | "+"  { PLUS }
  | "+." { PLUSDOT }
  | "*"  { STAR }
  | "-"  { MINUS }
  | "-." { MINUSDOT }
  | "->" { MINUSGREATER }
  | lowercase identchar *
      { let s = Lexing.lexeme lexbuf in
        try Hashtbl.find keyword_table s
        with Not_found -> LIDENT s }
  | uppercase identchar *
      { let s = Lexing.lexeme lexbuf in
        try Hashtbl.find ukeyword_table s
        with Not_found -> UIDENT s }
  | int_literal { INT (Lexing.lexeme lexbuf, None) }
  | (int_literal as lit) (literal_modifier as modif)
      { INT (lit, Some modif) }
  | float_literal | hex_float_literal
      { FLOAT (Lexing.lexeme lexbuf, None) }
  | ((float_literal | hex_float_literal) as lit) (literal_modifier as modif)
      { FLOAT (lit, Some modif) }
  | (float_literal | hex_float_literal | int_literal) identchar+
      { raise (Error(Invalid_literal (Lexing.lexeme lexbuf),
                     current_location lexbuf)) }
  | eof { EOF }
  | _
      { raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0),
                     current_location lexbuf))
      }

and comment n = parse
  | newline
         { update_loc lexbuf None ~line:1 ~absolute:false ~chars:0;
           comment n lexbuf }
  | "*)"
         { if n = 1 then ()
           else comment (n-1) lexbuf }
  | "(*"
         { comment (n+1) lexbuf }
  | _
         { comment n lexbuf }
