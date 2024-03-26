type t = Badd | Bsub | Bmul | Bdiv | Bmod

let string_of = function
  | Badd -> "+"
  | Bsub -> "-"
  | Bmul -> "*"
  | Bdiv -> "/"
  | Bmod -> "%"

let eval op x y =
  match op with
  | Badd -> x + y
  | Bsub -> x - y
  | Bmul -> x * y
  | Bdiv -> x / y
  | Bmod -> x mod y

