
(* The type of tokens. *)

type token = 
  | TIMES
  | RPAR
  | PLUS
  | MOD
  | MINUS
  | LPAR
  | INT of (int)
  | IDENT of (string)
  | EOF
  | DIV

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expression)
