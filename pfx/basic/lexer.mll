{
  open Parser 

  (* type token =
    | PUSH of int | POP | SWAP | ADD | SUB | MUL | DIV | MOD | INT of int | EOF *)

  let print_token = function
    | PUSH n -> print_string ("PUSH " ^ string_of_int n)
    | POP -> print_string "POP"
    | SWAP -> print_string "SWAP"
    | ADD -> print_string "ADD"
    | SUB -> print_string "SUB"
    | MUL -> print_string "MUL"
    | DIV -> print_string "DIV"
    | MOD -> print_string "MOD"
    | EOF -> print_string "EOF"
    | INT i -> print_int i

  


  (* let mk_int nb =
    try INT (int_of_string nb)
    with Failure _ -> failwith (Printf.sprintf "Illegal integer '%s': " nb)

  let mk_push nb =
    try PUSH (int_of_string nb)
    with Failure _ -> failwith (Printf.sprintf "Illegal push operation '%s': " nb ) *)

  (* Exercice 7: Locations *)

  let mk_int loc nb =
    try INT (int_of_string nb)
    with Failure _ -> raise (Utils.Location.Error (Printf.sprintf "Illegal integer '%s': " nb, loc))

  let mk_push loc nb =
    try PUSH (int_of_string nb)
    with Failure _ -> raise (Utils.Location.Error (Printf.sprintf "Illegal push operation '%s': " nb, loc))

    
}


let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let digit = ['0'-'9']

rule token = parse
  (* newlines *)
  | newline { token lexbuf }
  (* blanks *)
  | blank + { token lexbuf }
  (* end of file *)
  | eof      { EOF }
  (* comments *)
  | "--" not_newline_char*  { token lexbuf }
  (* integers *)
  | digit+ as nb            { mk_int (Utils.Location.symbol_loc (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)) nb }
  (* commands  *)
  | "push " + (digit+ as nb)  {mk_push (Utils.Location.symbol_loc (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)) nb  }
  | "pop"     { POP }
  | "swap"    { SWAP }
  | "add"     { ADD }
  | "sub"     { SUB }
  | "mul"     { MUL }
  | "div"     { DIV }
  | "mod"     { MOD }
  (* illegal characters *)
  | _ as c                  { failwith (Printf.sprintf "Illegal character '%c': " c) }

{
   let rec examine_all lexbuf =
    let result = token lexbuf in
    print_token result;
    print_string " ";
    match result with
    | EOF -> ()
    | _   -> examine_all lexbuf


  let compile file =
  print_string ("File "^file^" is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    examine_all lexbuf;
    print_newline ();
    close_in (input_file)
  with Sys_error _ ->
    print_endline ("Can't find file '" ^ file ^ "'")

let _ = Arg.parse [] compile ""
}