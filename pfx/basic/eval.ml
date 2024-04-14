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
  | [], _ -> Error ("Nothing to step",state)
  | Push op :: rest, stack -> Ok (rest, op :: stack)
  | Pop :: rest, stack ->
      (match stack with
       | _ :: stack' -> Ok (rest, stack')
       | [] -> Error ("Pop from empty stack", state))
  | Swap :: rest, stack ->
      (match stack with
       | op1 :: op2 :: stack' -> Ok (rest, op2 :: op1 :: stack')
       | _ -> Error("Swap with less than two elements", state))
  | Peek :: rest, stack ->
      (match stack with
        | [] -> Error ("Stack is empty", state)
        | x :: stack' -> Ok (rest, x :: stack'))
  | Operate op :: rest, stack ->
      (match stack with
       |  i1 ::  i2 :: stack' ->
           let result = 
             (match op with
             | Add -> Ok (rest, (i1 + i2) :: stack)
             | Sub -> Ok (rest, (i1 - i2) :: stack)
             | Mul -> Ok (rest, (i2 * i1) :: stack)
             | Div -> 
              if i2 = 0 then 
                Error("Division by zero", state) 
              else 
                Ok (rest, i1 / i2 :: stack')
             | Mod -> 
              if i2 = 0 then 
                Error("Division by zero", state) 
              else 
                Ok (rest, i1 mod i2 :: stack'))
           in result
       | _::[] -> Error ("Not enough integers on the stack", state)
       | [] -> Error ("Operate on non-integers", state))

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
