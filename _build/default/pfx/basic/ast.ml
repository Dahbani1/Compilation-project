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



type program = int * command list

(* add here all useful functions and types  related to the AST: for instance  string_of_ functions *)
let string_of_operand = function
  | Int i -> string_of_int i
  | Var v -> v

let string_of_operator = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"

let string_of_command = function
  | Push op -> "Push" ^ (string_of_operand op)
  | Pop -> "Pop"
  | Peek -> "Peek"
  | Swap -> "Swap"
  | Exec -> "Exec"
  | Operate op -> "Operate" ^ (string_of_operator op)

let string_of_commands cmds = String.concat " " (List.map string_of_command cmds)

let string_of_program (args, cmds) = Printf.sprintf "%i args: %s\n" args (string_of_commands cmds)

