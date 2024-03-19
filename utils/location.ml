open Lexing

type t =
  {
    loc_start: position;
    loc_end: position;
  }

(* a generic error exception carrying a message and a location *)
exception Error of string * t

let none =
  {
    loc_start = dummy_pos;
    loc_end = dummy_pos;
  }

let curr lexbuf = {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
}

let init lexbuf fname =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }

let incr_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
  {
    pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
  }

let symbol_loc startpos endpos =
  {
    loc_start = startpos;
    loc_end = endpos;
  }

let string_of loc =
  let debut = loc.loc_start
  and fin = loc.loc_end in
  "characters "^(string_of_int (debut.pos_cnum - debut.pos_bol))^"-"^(string_of_int (fin.pos_cnum - fin.pos_bol))

let print loc =
  let debut = loc.loc_start
  and fin = loc.loc_end in
  print_string ("File \"" ^ debut.pos_fname ^ "\", ");
  if (debut.pos_lnum = fin.pos_lnum) then
    begin
      print_string "line ";
      print_int debut.pos_lnum;
      print_string ", characters ";
      print_int (debut.pos_cnum - debut.pos_bol);
      print_string "-";
      print_int (fin.pos_cnum - fin.pos_bol)
    end
  else
    begin
      print_string "from line ";
      print_int debut.pos_lnum;
      print_string " character ";
      print_int (debut.pos_cnum - debut.pos_bol);
      print_string " to line ";
      print_int fin.pos_lnum;
      print_string " character ";
      print_int (fin.pos_cnum - fin.pos_bol)
    end;
  print_endline ":"

