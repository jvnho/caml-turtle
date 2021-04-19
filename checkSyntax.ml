(*check l'arbre de syntax (declaration, et instruction(expression))*)

open Syntax

exception Error of string

let var_list = ref []

let rec is_var_in_list elt list =
  match list with
  | [] -> false 
  | head::tail -> if head = elt then true else is_var_in_list elt tail

(*retourne rien, mais declenche une erreur en cas d'arbre incorecte*) 

let rec check_program = function 
  | Exp (d,s) -> check_program d; check_program s
  | Const n | Epsilone | HautPinceau | BasPinceau -> ()
  | Ident s -> if is_var_in_list s !var_list = true then raise (Error ("Variable already declared")) else var_list := s::(!var_list); ()
  | Parenthese e | Avance e | Moins e | Plus e | Tourne e -> check_program e
  | Affect (s, e) -> if id_var_in_list s !var_list = false then raise (Error ("Variable not declared")) else check_program e
  | DebutFin instr_list -> List.map (check_program instr_list)


