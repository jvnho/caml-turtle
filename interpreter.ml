(*
  si l'arbre passe le checkSyntaxe on l'interprete ici
  contient la gestion des etat des variable, et dessin
*)
open Syntax

let initialisation li_declaration = 
  List.map (fun ident -> (ident, 0)) li_declaration

let rec evaluation env expression = 
  match expression with
  |Exp (debut, suite)-> 
    begin
      let d = eval_debut env debut in
      match suite with
      |Moins exp -> d - (evaluation env exp)
      |Plus exp -> d + (evaluation env exp)
      |Epsilone -> d
    end
and eval_debut env debut = 
  match debut with
  |Const i -> i
  |Ident variable -> List.assoc variable env
  |Parenthese exp -> evaluation env exp

let exec_program arbre = failwith "erreur" 
