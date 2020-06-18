
(* The type of tokens. *)

type token = 
  | UNREACHABLE
  | UNDERSCORE
  | UIDENT of (string)
  | TAG
  | SWITCH
  | STUB
  | STAR
  | SEMICOLON
  | RPAREN
  | ROOT
  | REC
  | RBRACKET
  | RBRACE
  | PLUSDOT
  | PLUS
  | OPAQUE
  | MUT
  | MINUSGREATER
  | MINUSDOT
  | MINUS
  | LPAREN
  | LIDENT of (string)
  | LETK
  | LET
  | LBRACKET
  | LBRACE
  | IS_INT
  | INT of (string * char option)
  | IN
  | HCF
  | GET_FIELD
  | FLOAT of (string * char option)
  | EXN
  | EQUAL
  | EOF
  | EFFECT
  | DOT
  | DEF
  | CONT
  | COMMA
  | COLONEQUAL
  | COLON
  | CODE
  | CLOSURE
  | CCALL
  | BLOCK
  | BANG
  | AROBASE
  | APPLY
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Fexpr.program)
