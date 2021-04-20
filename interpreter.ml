(*
  si l'arbre passe le checkSyntaxe on l'interprete ici
  contient la gestion des etat des variable, et dessin
*)
open Syntax
exception Error of string

type position = {
  a: int;
  leve: bool;
  xmax: float;
  ymax: float;
}

let get_XY pos len = 
  let angle_rad = (float_of_int (!pos).a) *. (acos(-1.)/.180.) in
  let hypo = Float.of_int len in
  let dx = hypo *. (cos angle_rad) in
  let dy = hypo *. (sin angle_rad) in
  let posCourant = Graphics.current_point () in
  match posCourant with
  |(x, y) -> 
  (*
    print_string "\ncalcul distance--------\n";
    print_string "angle = ";
    print_int (!pos).a;
    print_string "\nhypo = ";
    print_float hypo;
    print_string "\n";
    print_int x;
    print_string ", ";
    print_int y;
    print_string "\nresul = ";
    *)

    let newX = (Float.of_int x) +. dx in
    let newY = (Float.of_int y) +. dy in
    if newX < 0. || newX > (!pos).xmax || newY < 0. || newY > (!pos).ymax then
      raise (Error "sortie de canvas")
    else(
      print_float newX;
      print_string ", ";
      print_float newY;
      print_string "\n";
      (newX, newY)
    )

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
    print_string "\nbas \n";
    (etat := {a = (!etat).a; leve=false; xmax=(!etat).xmax; ymax=(!etat).ymax};
    env)
  |Affect (variable, expression) -> 
    let e = evaluation env expression in
    print_int e;
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
        |(x, y)-> 
        if (!etat).leve then((Graphics.moveto (Float.to_int x) (Float.to_int y)); env)
                  else((Graphics.lineto (Float.to_int x) (Float.to_int y)); env)
      with
      |Error message -> raise (Error message)
    )
  |Tourne expression -> 
    let e = evaluation env expression in
    let newAngle = ((!etat).a + e) mod 360 in
    (etat := {a = newAngle; leve=(!etat).leve; xmax=(!etat).xmax; ymax=(!etat).ymax};
      env)
  |DebutFin li_instruction -> exec_li_instruction env li_instruction etat
  |TantQueFaire (expression, li_instruction) -> exec_do_while expression env li_instruction etat

and exec_li_instruction env li_instruction etat = 
  List.fold_left (fun environemnt instruction -> exec_instruction environemnt instruction etat) 
  env li_instruction

and exec_do_while expression env li_instruction etat =
  let e = evaluation env expression in 
  if e = 0 then env else exec_do_while expression (exec_li_instruction env li_instruction etat) li_instruction etat

let exec_program arbre = 
  let state = ref {a = 90; leve = true; xmax = 800.; ymax = 800.} in
  match arbre with 
  |(li_declaration, li_instruction)->
    let env = initialisation li_declaration in
    Graphics.open_graph " 800x800";
    Graphics.moveto 400 400;
    let _= exec_li_instruction env  li_instruction state in
    ignore (Graphics.wait_next_event [Button_down ; Key_pressed])
