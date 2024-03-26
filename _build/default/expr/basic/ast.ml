type expression =
  | Const of int
  | Var of string
  | Binop of BinOp.t * expression * expression
  | Uminus of expression

let rec string_of_expr exp =
  match exp with
  | Const c -> string_of_int c
  | Var v -> v
  | Binop(op, e1, e2) ->
      "(" ^(string_of_expr e1)^ (BinOp.string_of op) ^(string_of_expr e2)^ ")"
  | Uminus e -> "( -"^(string_of_expr e)^ ")"
