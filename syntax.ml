type expression =
Const of int
|Ident of string
|Parenthese of expression
|App of expression * op * expression
|UnaryMoins of expression
and op = 
Moins
|Plus
|Multi
|Div

type instruction = 
  | HautPinceau
  | BasPinceau
  | Affect of string * expression
  | Avance of expression
  | Tourne of expression
  | DebutFin of instruction list 
  | SiSinon of expression * instruction * instruction
  | TantQueFaire of expression * instruction 

type declaration = string 
type programme = declaration list * instruction list