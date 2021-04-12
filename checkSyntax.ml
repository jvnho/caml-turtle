(*check l'arbre de syntax (declaration, et instruction(expression))*)

exception Error of string

(*retourne rien, mais declanche une erreur en caas d'arbre incorecte*)
let check_program arbre = 
  raise (Error ("deux fois declaré")
