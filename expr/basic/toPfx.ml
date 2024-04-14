open Ast
open BinOp

let rec generate = function
  | Const n -> [BasicPfx.Ast.Push n]
  | Binop (op, e1, e2) -> generate e1 @ generate e2 @ [Operate (generate_op op)]
  | Uminus e -> generate e @ [Push 0; Operate Sub]
  | Var _ -> failwith "Not yet supported"

and generate_op = function
  | Badd -> Add
  | Bsub -> Sub
  | Bmul -> Mul
  | Bdiv -> Div
  | Bmod -> Mod