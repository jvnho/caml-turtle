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
  | HautPinceau | BasPinceau -> ()
  | Avance e | Tourne e -> check_expression e
  | Affect (s, e) -> if is_var_in_list s !var_list = false then raise (Error ("Variable not declared")) 
                    else check_expression e
  | DebutFin instr_list -> check_instruction_list instr_list
  | TantQueFaire (e, instr_list) -> check_expression e; check_instruction_list instr_list
  | SiSinon (e,instr1,instr2) -> check_expression e; check_program instr1; check_program instr2; 
and
check_instruction_list list = 
  match list with 
  |[] -> ()
  |head::tail -> check_program head; check_instruction_list tail;
and
check_expression = function
  | Exp (d,s) -> check_expression_debut d; check_expression_suite s
and 
check_expression_debut = function
  | Const _n -> ();
  | Ident s -> if is_var_in_list s !var_list = true then raise (Error ("Variable already declared")) 
              else var_list := s::(!var_list); ()
  | Parenthese e -> check_expression e
and 
check_expression_suite = function
  | Moins e -> check_expression e
  | Plus e -> check_expression e
  | Epsilone -> ()