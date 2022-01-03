 (* File parser.mly *)

(* On met les types utilis�s avec des alias entres "" *)

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
%token EOL

(* On �crit les r�gles de priorit� *)


%right "=>" "^" "v" NOT
%right "(" ")"

%start <int> main 

%%
(* d�finition des "r�gles de grammaire" *)

main:
| e = expr EOL { e }

expr :
| i = INT { i }
| "(" e = expr ")" { e }
| s = NAME { "s" } 
| NOT "(" e = expr ")" { Implies(e,False) } (* NOT prop <=> prop => False *)
| TRUE { True }
| FALSE { False }
| e1 = expr "=>" e2 = expr { Implies(e1,e2) }
| e1 = expr "^" e2 = expr { And(e1,e2) }
| e1 = expr "v" e2 = expr { Or(e1,e2) }


