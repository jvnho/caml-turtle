(*check l'arbre de syntax (declaration, et instruction(expression))*)

open Syntax

exception Error of string

let declaration_liste = ref [];;

let rec duplicate_elt list =
  match list with 
  |[] -> ""
  |head::tail ->  if List.exists ((=) head) tail then head else duplicate_elt tail
;;

let rec is_var_in_list elt list =
  match list with
  |[] -> false 
  |head::tail -> if head = elt then true else is_var_in_list elt tail
;;

let rec check_expression expression =
  match expression with
  |Const _n -> ()
  |Ident s -> if is_var_in_list s !declaration_liste = false then raise (Error ("Variable " ^s ^ " pas déclarée")) 
              else ()
  |Parenthese e -> check_expression e 
  |App (e1, _op, e2) -> check_expression e1; check_expression e2;
  |UnaryMoins e -> check_expression e;
;;

let rec check_instr_list list = 
  match list with 
  |[] -> ()
  |head::tail -> check_instr head; check_instr_list tail;

and
check_instr instruction = 
  match instruction with 
  |HautPinceau | BasPinceau -> ()
  |Avance e | Tourne e | Couleur e | Epaisseur e -> check_expression e
  |Affect (s, e) -> if is_var_in_list s !declaration_liste = false then raise (Error ("Variable " ^s ^ " pas déclarée")) 
                    else check_expression e
  |DebutFin instr_list -> check_instr_list instr_list
  |TantQueFaire (e, instr) -> check_expression e; check_instr instr
  |SiSinon (e,instr1,instr2) -> check_expression e; check_instr instr1; check_instr instr2; 
  |SiAlors (e, instr) -> check_expression e; check_instr instr
;;

(*retourne rien, mais declenche une erreur en cas d'arbre incorecte*)

let check_program arbre =
  match arbre with 
  |(li_declaration, li_instruction) ->
    declaration_liste := li_declaration; (*pour faireu une sorte de liste globale de declarations*)
    let dup = duplicate_elt !declaration_liste in
    if dup <> "" then raise (Error ("Variable " ^ dup ^ " déclarée deux fois"));
    check_instr_list li_instruction;