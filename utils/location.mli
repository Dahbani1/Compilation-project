(* A location in a file connected to a buffer *)
type t

(* a generic error exception carrying a message and a location *)
exception Error of string * t

(* A dummy location *)
val none : t

(* Initialization of the location of a buffer *)
(* The second argument is the name of the source file *)
val init : Lexing.lexbuf -> string -> unit

(* The current location of the buffer *)
val curr : Lexing.lexbuf -> t

(* Incrementation of the line number of the buffer (should be called
   whenever a newline is detected) *)
val incr_line : Lexing.lexbuf -> unit

(* A location from a start and an end *)
val symbol_loc : Lexing.position -> Lexing.position -> t

(* Printing of a location *)
val print : t -> unit
val string_of : t -> string
