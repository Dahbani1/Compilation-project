(* Exception that may be raised on error at run time by the function eval *)
exception RuntimeError of string

(* Function that evaluates an expression in a given environment *)
val eval : (string * int) list -> Ast.expression -> int
