(* backtrack.mli *)

val backtrack:  int -> (Proposition.t -> string) -> Proof.t -> bool * Proof.t
(* Tente de prouver le problème passé en paramètre
Arguments:
   verbosité (0 rien, 1 juste le résultat si ok, 2 tout)
-> fonction permettant de convertir une proposition en string
-> problème à prouver
Retourne:
   (réussite, nouvel état de preuve) *)
