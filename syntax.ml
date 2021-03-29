type expression = 
Const of int
|Ident of string
|Parenthese of expression
|Sequence of expression * suite
and suite =
Moins of expression
|Plus of expression
|Epsilone

type instruction = 
  | HautPinceau
  | BasPinceau
  | Affect of string * expression
  | Avance of expression
  | Tourne of expression
  | DebutFin of instruction list 

type declaration = string 
type programme = declaration list * instruction list