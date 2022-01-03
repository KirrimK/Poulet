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

%start main  (* peut-�tre rajouter un/des symbols de d�part *)

