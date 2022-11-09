(* Strategies.mli *)

val select_goal : int -> Proof.t -> bool * Proof.t
(* Permet de sélectionner un nouveau but courant 
Arguments: 
   identifiant du but souhaité 
-> état de preuve
Retourne:
   (réussite, nouvel état de preuve) *)

val intro : Proof.t -> bool * Proof.t
(* Utilisable si le but courant est de type A=>B. Place la proposition A dans les hypothèses et remplace le but courant par la proposition B 
Argument: 
   état de preuve
Retourne:
   (réussite, nouvel état de preuve)*)

val split : Proof.t -> bool * Proof.t
(* Utilisable si le but courant est de type A∧B. Remplace ce but par deux buts: A et B
Argument: 
   état de preuve
Retourne:
   (réussite, nouvel état de preuve) *)

val hyp_split : int -> Proof.t -> bool * Proof.t
(* Utilisable si l'hypothèse sélectionnée est de type A∧B. Sépare cette hypothèse et en crée deux nouvelles, qui contiennent respectivement A et B
Arguments:
   identifiant de l'hypothèse souhaitée 
-> état de preuve
Retourne:
   (réussite, nouvel état de preuve) *)

val left : Proof.t -> bool * Proof.t
(* Utilisable si le but courant est de type A∨B. Remplace ce but par A
Argument: 
   état de preuve
Retourne:
   (réussite, nouvel état de preuve) *)

val right : Proof.t -> bool * Proof.t
(* Utilisable si le but courant est de type A∨B. Remplace ce but par B
Argument: 
   état de preuve
Retourne:
   (réussite, nouvel état de preuve) *)

val hyp_left : int -> Proof.t -> bool * Proof.t
(* Utilisable si l'hypothèse sélectionnée est de type A∨B. Remplace cette hypothèse par A
Arguments:
   identifiant de l'hypothèse souhaitée 
-> état de preuve
Retourne:
   (réussite, nouvel état de preuve) *)

val hyp_right : int -> Proof.t -> bool * Proof.t
(* Utilisable si l'hypothèse sélectionnée est de type A∨B. Remplace cette hypothèse par B
Arguments:
   identifiant de l'hypothèse souhaitée 
-> état de preuve
Retourne:
   (réussite, nouvel état de preuve) *)

val false_hyp : int -> Proof.t -> bool * Proof.t
(* Prouve le but courant si l'hypothèse sélectionnée est "faux"
Arguments:
   identifiant de l'hypothèse souhaitée 
-> état de preuve
Retourne:
   (réussite, nouvel état de preuve) *)

val exact : int -> Proof.t -> bool * Proof.t
(* Prouve le but courant s'il est exactement le contenu de l'hypothèse sélectionnée
Arguments:
   identifiant de l'hypothèse souhaitée 
-> état de preuve
Retourne:
   (réussite, nouvel état de preuve) *)

val assumption : Proof.t -> bool * Proof.t
(* Prouve le but courant s'il est exactement le contenu de l'une des hypothèses
Argument:
   état de preuve
Retourne:
   (réussite, nouvel état de preuve) *)

val apply : int -> Proof.t -> bool * Proof.t
(* Utilisable si l'hypothèse sélectionnée est de type A=>B et si le but courant est B. Dans ce cas, le but courant devient A 
Arguments:
   identifiant de l'hypothèse souhaitée 
-> état de preuve
Retourne:
   (réussite, nouvel état de preuve) *)

val applyInHyp : bool -> int -> int -> Proof.t -> bool * Proof.t
(* Utilisable si la seconde hypothèse sélectionnée est de type A=>B et si la première hypothèse sélectionnée est A. Remplace celle-ci par B. Si l'argument "keep" est placé, la proposition A est gardée dans la liste
Arguments:
   "keep"
-> identifiant de l'hypothèse cible 
-> identifiant de l'hypothèse à appliquer
-> état de preuve
Retourne:
   (réussite, nouvel état de preuve) *)
