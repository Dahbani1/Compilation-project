open Ast
open BinOp

let generate = function
  | Const n -> [Push (Int n)]
  | Binop (op, e1, e2) -> generate e1 @ generate e2 @ [Operate (generate_op op)]
  | Uminuse -> generate e @ [Push (Int 0); Operate Sub]
  | Var _ -> failwith "Not yet supported"

and generate_op = function
  | Badd -> Add
  | Bsub -> Sub
  | Bmul -> Mul
  | Bdiv -> Div
  | Bmod -> Mod