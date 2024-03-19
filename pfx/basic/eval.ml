open Ast
open Printf

let string_of_stack stack = sprintf "[%s]" (String.concat ";" (List.map string_of_int stack))

let string_of_state (cmds,stack) =
  (match cmds with
   | [] -> "no command"
   | cmd::_ -> sprintf "executing %s" (string_of_command cmd))^
    (sprintf " with stack %s" (string_of_stack stack))

(* Question 4.2 *)
let step state =
  match state with
  | [], _ -> Error("Nothing to step",state)
  (* Valid configurations *)
  | Push op :: rest, stack -> Ok (rest, op :: stack)
  | Pop :: rest, stack ->
      (match stack with
       | _ :: stack' -> Ok (rest, stack')
       | [] -> Error("Pop from empty stack", state))
  | Peek :: rest, stack ->
      (match stack with
       | op :: _ -> (rest, op :: stack)
       | [] -> Error("Peek from empty stack", state))
  | Swap :: rest, stack ->
      (match stack with
       | op1 :: op2 :: stack' -> Ok (rest, op2 :: op1 :: stack')
       | _ -> Error("Swap with less than two elements", state))
  | Block cmds :: rest, stack -> Ok (cmds @ rest, stack)
  | Exec :: rest, stack ->
      (match stack with
       | Block cmds :: stack' -> Ok (cmds @ rest, stack')
       | _ -> Error("Exec non-block operand", state))
  | Operate op :: rest, stack ->
      (match stack with
       | Int i1 :: Int i2 :: stack' ->
           let result = 
             match op with
             | Add -> Int (i1 + i2)
             | Sub -> Int (i1 - i2)
             | Mul -> Int (i1 * i2)
             | Div -> if i2 = 0 then Error("Division by zero", state) else Ok (rest, Int (i1 / i2) :: stack')
           in result
       | _ -> Error("Operate on non-integers", state))

let eval_program (numargs, cmds) args =
  let rec execute = function
    | [], []    -> Ok None
    | [], v::_  -> Ok (Some v)
    | state ->
       begin
         match step state with
         | Ok s    -> execute s
         | Error e -> Error e
       end
  in
  if numargs = List.length args then
    match execute (cmds,args) with
    | Ok None -> printf "No result\n"
    | Ok(Some result) -> printf "= %i\n" result
    | Error(msg,s) -> printf "Raised error %s in state %s\n" msg (string_of_state s)
  else printf "Raised error \nMismatch between expected and actual number of args\n"
