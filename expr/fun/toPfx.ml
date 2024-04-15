open Ast
open BinOp

let rec generate env depth = function
  | Const n -> [BasicPfx.Ast.Push n]
  | Var x -> [BasicPfx.Ast.Access (List.assoc x env)]
  | Binop (op, e1, e2) -> generate env depth e1 @ generate env depth e2 @ [Operate (generate_op op)]
  | Uminus e -> generate env depth e @ [Push 0; Operate Sub]
  | Fun (param, body) ->
    let body_cmds, _ = generate ((param, depth) :: env) (depth + 1) body lexbuf in
    [PUSHSEQ(body_cmds, lexbuf)], depth
  | App (e1, e2) ->
    let arg_cmds, depth1 = generate env depth e2 lexbuf in
    let fun_cmds, depth2 = generate env depth1 e1 lexbuf in
    arg_cmds @ fun_cmds @ [EXEC(lexbuf); SWAP(lexbuf); POP(lexbuf)], depth2 - 1

and generate_op = function
  | Badd -> Add
  | Bsub -> Sub
  | Bmul -> Mul
  | Bdiv -> Div
  | Bmod -> Mod
