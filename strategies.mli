(* Strategies.mli *)

val fail : Proof.t -> bool * Proof.t
(* Fonction appelée pour indiquer que la stratégie exécutée est inutilisable *)

val select_goal : int -> Proof.t -> bool * Proof.t
(* Permet de sélectionner un nouveau but courant *)

val intro : Proof.t -> bool * Proof.t
(* Utilisable si le but courant est de type A=>B. Place la proposition A dans les hypothèses et remplace le but courant par la proposition B *)

val split : Proof.t -> bool * Proof.t
(* Utilisable si le but courant est de type A∧B. Remplace ce but par deux buts: A et B *)

val hyp_split : int -> Proof.t -> bool * Proof.t
(* Utilisable si l'hypothèse sélectionnée est de type A∧B. Sépare cette hypothèse et en crée deux nouvelles, qui contiennent respectivement A et B *)

val left : Proof.t -> bool * Proof.t
(* Utilisable si le but courant est de type A∨B. Remplace ce but par A *)

val right : Proof.t -> bool * Proof.t
(* Utilisable si le but courant est de type A∨B. Remplace ce but par B *)

val hyp_left : int -> Proof.t -> bool * Proof.t
(* Utilisable si l'hypothèse sélectionnée est de type A∨B. Remplace cette hypothèse par A *)

val hyp_right : int -> Proof.t -> bool * Proof.t
(* Utilisable si l'hypothèse sélectionnée est de type A∨B. Remplace cette hypothèse par B *)

val false_hyp : int -> Proof.t -> bool * Proof.t
(* Prouve le but courant si l'hypothèse sélectionnée est "faux" *)

val exact : int -> Proof.t -> bool * Proof.t
(* Prouve le but courant s'il est exactement le contenu de l'hypothèse sélectionnée *)

val assumption : Proof.t -> bool * Proof.t
(* Prouve le but courant s'il est exactement le contenu de l'une des hypothèses *)

val apply : int -> Proof.t -> bool * Proof.t
(* Utilisable si l'hypothèse sélectionnée est de type A=>B et si le but courant est B. Dans ce cas, le but courant devient A *)

val applyInHyp : bool -> int -> int -> Proof.t -> bool * Proof.t
(* Utilisable si la seconde hypothèse sélectionnée est de type A=>B et si la première hypothèse sélectionnée est A. Remplace celle-ci par B. Si l'argument "keep" est placé, la proposition A est gardée *)
