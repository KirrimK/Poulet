 (* File parser.mly *)

(* On met les types utilisés avec des alias entres "" *)

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

(* On écrit les règles de priorité *)


%right "=>" "^" "v" NOT
%right "(" ")"

%start main  (* peut-être rajouter un/des symbols de départ *)

