(* The type of the commands for the stack machine *)
type operator =
  | Add
  | Sub
  | Mul
  | Div
  | Mod

type command =
| Push of int
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
