type elementExpression = 
Const of int
|Ident of string

type expression = 
elementExpression
|Parenthese of expression
|Sequence of elementExpression * suite
and suite =
Moins of expression
|Plus of expression
|Epsilon

type instruction = 
  | HautPinceau
  | BasPinceau
  | Affect of string * expression
  | Avance of expression
  | Tourne of expression
  | DebutFin of instruction list 

type declaration = string 
type programme = declaration list * instruction list