{
    open Parser
    exception Lexing_error of string
}

let space = [' ' '\n' '\t']
let nombre = [1-9][0-9]* | 0
let identificateur = [a-z][a-zA-Z0-9]*

rule programme = parse 
    | "Var"                     {declaration Lexing.lexeme lexbuf}
    | space*                    {programme Lexing.lexeme lexbuf}
    | _                         {instruction Lexing.lexeme lexbuf}

and declaration = parse 
    | "Var"                     {VAR}
    | identificateur            {INDENT}
    | ";"                       {POINTVIRGULE}
    | space+                    {declaration Lexing.lexeme lexbuf}
    | _                         {programme Lexing.lexeme lexbuf}

and instruction = parse 
    | "Avance"                  {AVANCE}
    | "Tourne"                  {TOURNE}
    | "BasPinceau"              {BASPINCEAU}
    | "HautPinceau"             {HAUTPINCEAU}
    | nombre                    {INTCONST}
    | identificateur            {IDENT}
    | "("                       {LEFTPA}
    | ")"                       {RIGHTPA}
    | "+"                       {PLUS}
    | "-"                       {MOINS} 
    | "="                       {EGALE}
    | "Debut"                   {DEBUT}
    | "Fin"                     {FIN}
    | space+                    {instruction Lexing.lexeme lexbuf}
    | eof                       {EOF}
    | _                         {raise(Lexing_error(Lexing.lexeme lexbuf))}
