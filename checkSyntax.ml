(*check l'arbre de syntax (declaration, et instruction(expression))*)

exception Error of string

let var_list = ref []

let is_var_in_list elt list =
  match list with
  | [] -> False 
  | head::tail -> if head = elt then True else is_var_in_list elt tail

(*retourne rien, mais declanche une erreur en caas d'arbre incorecte*)
let rec check_program = function 
    | Exp (d,s) -> check_program d; check_program s
    | Const n | Epsilone | HautPinceau | BasPinceau -> ()
    | Ident s -> if is_var_in_list s !var_list = True then raise (Error ("Variable already declared")) else var_list := s::(!var_list); ()
    | Parenthese e | Avance e | Moins e | Plus e | Tourne e -> check_program e
    | Affect (s, e) -> if id_var_in_list s !var_list = False then raise (Error ("Variable not declared")) else check_program e
    | DebutFin instr_list -> List.map (check_program instr_list)


