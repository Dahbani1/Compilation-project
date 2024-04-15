open Ast

exception RuntimeError of string

let rec eval env = function
  | Const c -> c
  | Var v -> (try List.assoc v env with Not_found -> raise(RuntimeError("Unbound variable "^v)))
  | Binop(op,e1,e2) ->
     begin
       match op,eval env e2 with
       | (Bdiv | Bmod), 0 -> raise(RuntimeError("division by zero"))
       | _, v -> (BinOp.eval op) (eval env e1) v
     end
  | Uminus e -> - (eval env e)
  | Lambda (x, e) -> fun v -> eval ((x, v)::env) e  (* Lambda abstraction creates a new function *)
  | App (e1, e2) -> (eval env e1) (eval env e2)  (* Application applies a function to an argument *)
