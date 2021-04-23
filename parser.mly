%token VAR POINTVIRGULE AVANCE TOURNE BASPINCEAU HAUTPINCEAU EGALE COULEUR EPAISSEUR
%token DEBUT FIN EOF LEFTPA RIGHTPA SI ALORS SINON WHILE DO PLUS MOINS MULTI DIV
%left MOINS PLUS
%right MULTI DIV
%token <int> INTCONST
%token <string> IDENT
%nonassoc ALORS
%nonassoc SINON
%start <Syntax.programme> s
%{open Syntax%}
%%
s: p = programme EOF {p}

programme: d = declaration* i=instruction* {(d, i)}

declaration: 
VAR i=IDENT POINTVIRGULE {i}

instruction:
AVANCE e=expression { Avance e }
| TOURNE e=expression { Tourne e }
| BASPINCEAU { BasPinceau }
| HAUTPINCEAU { HautPinceau }
| i=IDENT EGALE e=expression { Affect(i, e) }
| DEBUT b=blocInstruction FIN { DebutFin b }
| SI e=expression ALORS i1=instruction SINON i2=instruction { SiSinon(e, i1, i2) }
| WHILE e=expression DO i=instruction {TantQueFaire(e,i)}
| SI e=expression ALORS i= instruction {SiAlors(e, i)}
| COULEUR e = expression {Couleur(e)}
| EPAISSEUR e = expression {Epaisseur(e)}

blocInstruction:
i=instruction POINTVIRGULE b=blocInstruction {i::b}
| {[]}

expression:
n=INTCONST { (Const n) }
|i=IDENT { (Ident i) }
|LEFTPA e=expression RIGHTPA { Parenthese(e) }
|e1 = expression o=operation e2 = expression { App(e1,o,e2) }
|MOINS e= expression { UnaryMoins(e)}

%inline operation:
|PLUS { Plus }
|MOINS { Moins }
|MULTI { Multi }
|DIV { Div }