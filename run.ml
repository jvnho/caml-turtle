(*programme principale*)

let print_position outx lexbuf =
  Lexing.(
    let pos = lexbuf.lex_curr_p in
    Printf.fprintf outx "Ligne %d Col %d"
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
  )

let _=
  let lb = Lexing.from_channel stdin in
  try
    let arbre = Parser.s Lexer.token lb in
    CheckSyntax.check_program ast;
    Interpreter.exec_program ast
  with
  | Lexer.Error msg ->
     Printf.fprintf stderr "%a: Lexer error reading %s\n" print_position lb msg;
     exit (-1)
  | Parser.Error ->
     Printf.fprintf stderr "%a: Syntax error\n" print_position lb;
     exit (-1)
  | CheckSyntax.Error s ->
     Printf.fprintf stderr "Type error: %s\n" s;
     exit (-1)