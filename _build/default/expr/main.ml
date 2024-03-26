(* Entry point of the program, should contain your main function: here it is
 named parse_eval, it is the function provided after question 6.1 *)
open BasicExpr
open Utils

(* The main function *)
let parse_eval file =
  print_string ("\nFile "^file^" is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    begin
      try
        let expr_prog = Parser.expression Lexer.token lexbuf in
        let result = Eval.eval [] expr_prog in
        Printf.printf "%s = %i\n" (Ast.string_of_expr expr_prog) result
      with
      | Parser.Error ->
         print_string "Syntax error: ";
         Location.print (Location.curr lexbuf)
      | Location.Error(e,l) ->
         print_string e;
         Location.print l
    end;
    close_in (input_file)
  with Sys_error _ ->
    print_endline ("Can't find file '" ^ file ^ "'")

(* Here we add the parsing of the command line and link to the main function *)
let _ =
  Arg.parse [] parse_eval ""
