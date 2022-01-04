%{ open Strategies %}
%token <int> INT
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

(* On écrit les règles de priorité *)

%right "v"
%right "=>" "^"
%nonassoc NOT

%start <Strategies.proposition> main

%%
(* définition des "règles de grammaire" *)

main:
| e = expr EOL { e }

expr :
/* | i = INT { i } */
| "(" e = expr ")" { e }
| s = NAME { p_name s }
| NOT e = expr { p_not e } (* NOT prop <=> prop => False *)
| TRUE { p_true }
| FALSE { p_false }
| e1 = expr "=>" e2 = expr { e1 => e2 }
| e1 = expr "^" e2 = expr { e1 ^ e2 }
| e1 = expr "v" e2 = expr { e1 $ e2 }
