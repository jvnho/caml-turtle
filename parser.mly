%token VAR POINTVIRGULE AVANCE TOURNE BASPINCEAU HAUTPINCEAU EGALE 
%token DEBUT FIN PLUS MOINS EOF LEFTPA RIGHTPA
%token <int> INTCONST
%token <string> IDENT
%start <Syntax.programme> s
%{open Syntax%}
%%
s: p = programme EOF {p}

programme: d = declaration* i=instruction* {(d, i)}

declaration: 
VAR i=IDENT POINTVIRGULE {i}

instruction:
AVANCE e=expression { Avance e }
| TOURNE e=expression { Toune e }
| BASPINCEAU { BasPinceau }
| HAUTPINCEAU { HautPinceau }
| i=IDENT EGALE e=expression { Affect(i, e)}
| DEBUT b=blocInstruction FIN {DebutFin b}

blocInstruction:
i=instruction POINTVIRGULE b=blocInstruction {i::blocInstruction}
| {[]}

expression:
n=INTCONST e=expressionSuite {Exp((Const n), e)}
|i=IDENT e=expressionSuite {Exp((Ident i), e)}
|LEFTPA e=expression RIGHTPA e2 = expressionSuite {Exp(Parenthese(e), e2)}

expressionSuite:
PLUS e=expression  {Plus e}
|MOINS e=expression {Moins e}
| {Epsilone}

