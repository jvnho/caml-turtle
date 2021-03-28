type op = Plus | Moins

type expression = 
  Const of int
  |Ident of string
  |App of expression * op * expression

type instruction = 
  | Haut
  | Bas
  | Affect of string * expression
  | Avance of expression
  | Tourne of expression
  | DebutFin of instruction list 

type declaration = string 
type program = declaration list * instruction list