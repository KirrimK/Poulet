(* fichier parser.mly*)


%{ open Proposition %}
%token <string> NAME
%token TRUE
%token FALSE
%token IMPLIES "=>"
%token AND "^"
%token OR "v"
%token NOT
%token LPAREN "("
%token RPAREN ")"
%token EOL EOF

(* On �crit les r�gles de priorit� *)

%right "v"  (* "ou" moins prioritaire que "et" et "=>" *)
%right "=>" "^"
%nonassoc NOT (* op�rateur unaire donc non associatif *)

%start <Strategies.proposition> main

%%
(* d�finition des "r�gles de grammaire" *)

main:
| e = expr EOL { e }

expr :
| "(" e = expr ")" { e }
| s = NAME { p_name s }
| NOT e = expr { p_not e } (* NOT prop <=> prop => False *)
| TRUE { p_true }
| FALSE { p_false }
| e1 = expr "=>" e2 = expr { e1 => e2 }
| e1 = expr "^" e2 = expr { e1 ^ e2 }
| e1 = expr "v" e2 = expr { e1 $ e2 }
