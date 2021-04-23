(*
  si l'arbre passe le checkSyntaxe on l'interprete ici
  contient la gestion des etat des variable, et dessin
*)

open Syntax
exception Error of string

(*etat du canvas*)
type position = {
  a: int;
  leve: bool;
  xmax: float;
  ymax: float;
}
(*->nouvelle position, si on se deplace de len*)
let get_XY pos len = 
  let angle_rad = (float_of_int (!pos).a) *. (acos(-1.)/.180.) in
  let hypo = Float.of_int len in
  let dx = hypo *. (cos angle_rad) in
  let dy = hypo *. (sin angle_rad) in
  let posCourant = Graphics.current_point () in
  match posCourant with
  |(x, y) -> 
    let newX = (Float.of_int x) +. dx in
    let newY = (Float.of_int y) +. dy in
    if newX < 0. || newX > (!pos).xmax || newY < 0. || newY > (!pos).ymax then
      raise (Error "sortie de canvas")
    else (newX, newY)

(*initialise les variable a zero*)
let initialisation li_declaration = 
  List.map (fun ident -> (ident, 0)) li_declaration

(*resultat entiere de l'expression*)
let rec evaluation env expression = 
  match expression with
  |Const i -> i
  |Ident variable -> List.assoc variable env
  |Parenthese exp -> evaluation env exp
  |App (expression1, operation, expression2)->
    (
      let e1 = evaluation env expression1 in
      let e2 = evaluation env expression2 in
      match operation with
      |Moins -> e1 - e2
      |Plus -> e1 + e2
      |Multi -> e1 * e2
      |Div -> if e2 = 0 then raise (Error "Division par zero") else e1/e2
    )
  |UnaryMoins exp -> -(evaluation env exp)


(*renvoie la nouvelle environement apres l'execution de l'instruction *)
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
  |SiSinon (expression, instructionAlors, instructionSinon) ->
    let e = evaluation env expression in
    if e <> 0 then exec_instruction env instructionAlors etat 
    else exec_instruction env instructionSinon etat
  |TantQueFaire (expression, instr) -> 
    let e = evaluation env expression in 
    if e = 0 then env else 
      let env2 = exec_instruction env instr etat in
      exec_instruction env2 instruction etat 
  |SiAlors (expression, instruction) -> 
    let e = evaluation env expression in
    if e<>0 then exec_instruction env instruction etat else env
  | Couleur (expr1,expr2,expr3) ->
    let e1 = evaluation env expr1 in 
    let e2 = evaluation env expr2 in 
    let e3 = evaluation env expr3 in 
    if e1 > 255 || e1 < 0 || e2 > 255 || e2 < 0 || e3 > 255 || e3 < 0  then raise(Error "Code Couleur RGB doit être compris entre 0 et 255 inclus");
    (Graphics.set_color (Graphics.rgb e1 e2 e3); env)
  | Epaisseur expression ->
    let e = evaluation env expression in 
    if e <= 0 then raise(Error "Epaisseur doit être strictement supérieur à zéro");
    (Graphics.set_line_width e; env)

(*execution de plusieur instruction*)
and exec_li_instruction env li_instruction etat = 
  List.fold_left (fun environemnt instruction -> exec_instruction environemnt instruction etat) 
  env li_instruction

(*fonction principale*)
let exec_program arbre = 
  let state = ref {a = 90; leve = true; xmax = 800.; ymax = 800.} in
  match arbre with 
  |(li_declaration, li_instruction)->
    let env = initialisation li_declaration in
    Graphics.open_graph " 800x800";
    Graphics.moveto 0 0;
    let _= exec_li_instruction env  li_instruction state in
    ignore (Graphics.wait_next_event [Button_down ; Key_pressed])
