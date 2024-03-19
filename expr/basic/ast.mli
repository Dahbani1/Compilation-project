(* The expression type *)
type expression =
    Const of int
  | Var of string
  | Binop of BinOp.t * expression * expression
  | Uminus of expression

(* Converting an expression to a string for printing *)
val string_of_expr : expression -> string
