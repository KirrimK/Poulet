(* random_props.mli *)

val propAleatoire : int -> Proposition.t;;
(* Argument:
      profondeur maximale
   Retourne:
      proposition aléatoire *)

val add_rand_goal : int -> Proof.t -> bool * Proof.t;;
(* Ajoute un but aléatoire à un état de preuve
Arguments:
   profondeur maximale
-> état de preuve
Retourne:
   (réussite, nouvel état de preuve) *)

val add_rand_cont : int -> int -> Proof.t -> bool * Proof.t;;
(* Ajoute des hypothèses aléatoires à la liste d'hypothèses du but courant
Arguments:
   profondeur maximale
-> nombres d'hypothèses à ajouter
-> état de preuve
Retourne:
   (réussite, nouvel état de preuve) *)
