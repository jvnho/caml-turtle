(*
  si l'arbre passe le checkSyntaxe on l'interprete ici
  contient la gestion des etat des variable, et dessin
*)
open Syntax
exception Error of string

type position = {
  a: float;
  leve: bool;
  xmax: float;
  ymax: float;
}

let get_XY pos len = 
  let hypo = Float.of_int len in
  let dx = hypo *. (cos (!pos).a) in
  let dy = hypo *. (sin (!pos).a) in
  let posCourant = Graphics.current_point () in
  match posCourant with
  |(x, y) -> 
    let newX = (Float.of_int x) +. dx in
    let newY = (Float.of_int y) +. dy in
    if newX < 0. || newX > (!pos).xmax || newY < 0. || newY > (!pos).ymax then
      raise (Error "sortie de canvas")
    else 
      (newX, newY)

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

let rec exec_instruction env instruction etat = 
  match instruction with 
  |HautPinceau -> 
    (etat := {a = (!etat).a; leve=true; xmax=(!etat).xmax; ymax=(!etat).ymax};
    env)
  |BasPinceau ->
    (etat := {a = (!etat).a; leve=false; xmax=(!etat).xmax; ymax=(!etat).ymax};
    env)
  |Affect (variable, expression) -> 
    let e = evaluation env expression in
    List.map (fun element -> 
      match element with
      |(ident, valeur)-> if ident=variable then (ident, e) else (ident, valeur)
    )env
  |Avance expression -> 
    let e = evaluation env expression in
    (
      try 
        let pos2 = get_XY etat e in
        match pos2 with
        |(x, y)-> if (!etat).leve then((Graphics.moveto (Float.to_int x) (Float.to_int y)); env)
                  else((Graphics.lineto (Float.to_int x) (Float.to_int y)); env)
      with
      |Error message -> raise (Error message)
    )
  |Tourne expression -> 
    let e = evaluation env expression in
    let newAngle = (!etat).a +. Float.of_int e in
    (etat := {a = newAngle; leve=(!etat).leve; xmax=(!etat).xmax; ymax=(!etat).ymax};
      env)
  |DebutFin li_instruction -> exec_li_instruction env li_instruction etat

and exec_li_instruction env li_instruction etat = 
  List.fold_left (fun environemnt instruction -> exec_instruction environemnt instruction etat) 
  env li_instruction

let exec_program arbre = 
  let state = ref {a = 90.; leve = true; xmax = 0.; ymax = 0.} in
  match arbre with 
  |(li_declaration, li_instruction)->
    let env = initialisation li_declaration in
    Graphics.open_graph " 800x800";
    exec_li_instruction env  li_instruction state
