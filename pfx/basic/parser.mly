%{
 open Ast

%}

(**************
 * The tokens *
 **************)

(* enter tokens here, they should begin with %token *)
%token EOF
%token <int> INT
%token <int> PUSH                  (* Push command *)
%token POP                  (* Pop command *)
%token ADD                  (* Add command *)
%token SUB                  (* Subtract command *)
%token MUL                  (* Multiply command *)
%token DIV                  (* Divide command *)
%token MOD                  (* Modulo command *)

(******************************
 * Entry points of the parser *
 ******************************)

(* enter your %start clause here *)
%start <Ast.program> program

%%

(*************
 * The rules *
 *************)

(* list all rules composing your grammar; obviously your entry point has to be present *)

program:
  | INT command_list EOF { $1, $2 }     (* A program consists of an integer and a list of commands followed by end-of-file *)


command_list:
  | /* Empty */ { [] }          (* An empty command list *)
  | command command_list { $1 :: $2 }   (* A command followed by another command list *)

command:
  | PUSH INT { Push $2 }
  | POP      { Pop }
  | ADD      { Operate Add }
  | SUB      { Operate Sub }
  | MUL      { Operate Mul }
  | DIV      { Operate Div }
  | MOD      { Operate Mod }
%%
