type a

type t = a list

val empty : t

val get_hyps : t -> Proposition.t list
(* Argument:
      état de preuve
   Retourne:
      liste des hypothèses du but courant*)

val get_goal : t -> Proposition.t list
(* Argument:
      état de preuve
   Retourne:
      liste des buts à prouver*)

val hyp_ids : t -> int list

val goal_ids : t -> int list

val make_proof : Proposition.t list list -> Proposition.t list -> t
(* Arguments:
      liste des listes d'hypothèses correspondant à chaque but
   -> liste des buts à prouver
   Retourne:
      état de preuve formé par ces deux listes *)

val demake_proof : t -> Proposition.t list list * Proposition.t list
(* Fonction réciproque de make_proof *)

val make_a : Proposition.t list -> Proposition.t -> a
(* Arguments:
      liste d'hypothèses
   -> but
   Retourne:
      enregistrement formé par les deux arguments *)

val get_hyp : int -> t -> Proposition.t
(* Permet d'obtenir une hypothèse du but courant
Arguments:
   identifiant de l'hypothèse souhaitée
-> état de preuve
Retourne:
   hypothèse souhaitée *)

val add_goal : Proposition.t -> t -> t
(* Permet d'ajouter une proposition à la liste des buts à prouver
Arguments:
   proposition à rajouter
-> état de preuve
Retourne:
   nouvel état de preuve *)

val add_hyp_to_a : Proposition.t -> a -> a
(* Permet d'ajouter une proposition  à la liste d'hypothèses d'un enregistrement 
Arguments:
   proposition à rajouter
-> enregistrement
Retourne:
   nouvel enregistrement *)

val add_hyp : Proposition.t -> t -> t
(* Permet d'ajouter une proposition à la liste des hypothèses du but courant 
Arguments:
   proposition à rajouter
-> état de preuve
Retourne:
   ouvel état de preuve *)

val get_first_goal : t -> Proposition.t
(* Argument:
      état de preuve
   Retourne:
      but courant *)

val change_first_goal : Proposition.t -> t -> t
(* Permet de modifier le but courant
Arguments:
   nouveau but courant
-> état de preuve
Retourne:
   nouvel état de preuve *)

val is_proven : t -> bool
(* Argument:
      état de preuve
   Retourne:
      "Vrai" s'il n'y a pas de buts à prouver, "Faux" sinon *)

val remove_hyp : int -> t -> Proposition.t list
(* Permet de supprimer une hypothèse de la liste des hypothèses du but courant 
Arguments:
   hypothèse à supprimer
-> état de preuve
Retourne:
   nouvelle liste des hypothèses du but courant *)

val remove_item_list : int -> 'a list -> 'a list
(* Permet de supprimer un élément d'une liste
Arguments:
   élément à supprimer
-> liste
Retourne:
   nouvelle liste *)

val place_elt_at_head : int -> 'a list -> 'a list
(* Permet de déplacer un élément en tête de liste
Arguments:
   élément à déplacer
-> liste
Retourne:
   nouvelle liste *)

val rm_duplic : 'a list -> 'a list

val clean : t -> t
(* Supprime les doublons dans les hypothèses et les buts ainsi que les buts égaux à "Vrai"
Argument:
   état de preuve
Retourne:
   nouvel état de preuve *)

val proof_goal_depth : t -> int

val proof_goal_items : t -> int
