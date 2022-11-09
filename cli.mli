(* cli.mli *)

val prop_to_string: Proposition.t -> string;;
(* Convertit une proposition en une <chaîne de caractères *)

val string_to_list: string -> string list;;
(* Renvoie la liste <des sous-chaînes de la chaîne de caractères passée en argument, délimitées par un espace *)

val proof_to_string: Proof.t -> string;;
(* Convertit un état de preuve en une chaîne de caractères *) 

val repl: unit -> unit;;
(* REPL: Read-Eval-Print Loop
   Analyse les commandes entrées par l'utilisateur pour en extraire des stratégies à appliquer sur l'état de preuve *)



