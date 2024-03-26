type t = Badd | Bsub | Bmul | Bdiv | Bmod

val string_of : t -> string

val eval : t -> int -> int -> int
