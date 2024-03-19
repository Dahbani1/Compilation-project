(* The type of the commands for the stack machine *)
type operand =
  | Int of int
  | Var of string

type operator =
  | Add
  | Sub
  | Mul
  | Div
  | Mod

type command =
| Push of operand
| Pop
| Peek
| Swap
| Exec
| Operate of operator
  

(* The type for programs *)
type program = int * command list

(* Converting a command to a string for printing *)
val string_of_command : command -> string

(* Converting a program to a string for printing *)
val string_of_program : program -> string
