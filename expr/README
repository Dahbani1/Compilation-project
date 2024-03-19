You can test the provided code in utop by

dune utop .

and then, for example

utop # open BasicExpr;;
utop # let expr_prog = Parser.expression Lexer.token (Lexing.from_string "5 + 6");;
val expr_prog : Ast.expression =
  BasicExpr.Ast.Binop (BinOp.Badd, BasicExpr.Ast.Const 5,
   BasicExpr.Ast.Const 6)
utop # Ast.string_of_expr expr_prog;;
- : string = "(5+6)"
utop # Eval.eval [] expr_prog;;
- : int = 11
