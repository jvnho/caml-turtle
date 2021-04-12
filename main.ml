open Token;;
open Lexer;;

let ch = open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel ch in
    while true do
      let t = Lexer.programme lexbuf in
      match t with
      | EOF -> print_string(to_string t);print_newline();exit 0
      | _ -> print_string (to_string t);print_newline()
    done                              
                           
