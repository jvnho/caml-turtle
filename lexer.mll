{
    open Lexing

    open Parser

    exception Error of string

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
        { pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
        }
}

rule programme = parse 
    | [' ''\t']                 { programme lexbuf }
    | '\n'                      { next_line lexbuf; programme lexbuf }
    | "Avance"                  { AVANCE }
    | "Tourne"                  { TOURNE }
    | "BasPinceau"              { BASPINCEAU }
    | "HautPinceau"             { HAUTPINCEAU }
    | "Var"                     { VAR }
    | "Debut"                   { DEBUT }
    | "Fin"                     { FIN }
    | "Si"                      { SI }
    | "Alors"                   { ALORS }
    | "Sinon"                   { SINON }
    | "Tant que"                { WHILE }
    | "Faire"                   { DO }
    | ['0'-'9']+ as i           { INTCONST (int_of_string i) }
    | ['a'-'z']+ as s           { IDENT s }
    | ";"                       { POINTVIRGULE }
    | "("                       { LEFTPA }
    | ")"                       { RIGHTPA }
    | "+"                       { PLUS }
    | "-"                       { MOINS } 
    | "/"                       { DIV }
    | "*"                       { MULTI }
    | "="                       { EGALE }
    | eof                       { EOF }
    | _                         { raise(Error(Lexing.lexeme lexbuf))}