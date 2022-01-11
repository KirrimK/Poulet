type t

val p_true : t
(* Proposition égale au vrai logique *)

val p_false : t
(* Proposition égale au faux logique *)

val p_name : string -> t
(* Constructeur d'une proposition de nom <argument>
   Argument: nom de la proposition
   Retourne: la proposition *)

val p_not : t -> t
(* Constructeur de la négation d'une proposition
(sucre syntaxique: remplacé par (proposition => p_false)
   Argument: la proposition
   Retourne: sa négation *)

val ( => ) : t -> t -> t
(* Constructeur de l'implication de deux propositions
   Arguments: proposition à gauche de =>
   -> proposition à droite de =>
   Retourne: l'implication gauche => droite *)

val ( ^ ) : t -> t -> t
(* Constructeur du ET logique entre deux propositions
   Arguments: proposition gauche -> proposition droite
   Retourne: ET logique gauche ^ droite *)

val ( $ ) : t -> t -> t
(* Constructeur du OU logique entre deux propositions
   Arguments: proposition gauche -> proposition droite
   Retourne: OU logique gauche v droite *)

exception Invalid_Input

val prop_match : (string -> 'a) -> 'a -> 'a -> (t -> t -> 'a) -> (t -> t -> 'a) -> (t -> t -> 'a) -> t -> 'a
(* Abstraction du pattern-matching sur le noeud racine d'une proposition
   Arguments:
   fonction applicable sur une proposition nommée
-> constante à renvoyer si Vrai logique
-> constante à renvoyer si Faux logique
-> fonction applicable sur une implication
-> fonction applicable sur un ET logique
-> fonction applicable sur un OU logique
-> proposition à matcher
   Retourne: 'a renvoyé par l'une des fonctions en argument *)

val prop_iter : (string -> 'a) -> 'a -> 'a -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> t -> 'a
(* Itérateur récursif parcourant l'intégralité de la proposition
   Arguments:
   fonction applicable sur une proposition nommée
-> constante à renvoyer si Vrai logique
-> constante à renvoyer si Faux logique
-> fonction applicable sur une implication
-> fonction applicable sur un ET logique
-> fonction applicable sur un OU logique
-> proposition à matcher
   Retourne: 'a *)

(* Abstractions du pattern-matching restreintes à un type de noeud à la fois *)
val p_matchname : (string -> 'a) -> 'a -> t -> 'a

val p_matchtrue : 'a -> 'a -> t -> 'a

val p_matchfalse : 'a -> 'a -> t -> 'a

val p_matchimpl : (t -> t -> 'a) -> 'a -> t -> 'a

val p_matchand : (t -> t -> 'a) -> 'a -> t -> 'a

val p_matchor : (t -> t -> 'a) -> 'a -> t -> 'a
(* --------------- *)

val polo_prop : string list -> t
(* Construction d'une proposition à l'aide de la notation polonaise inversée
ex: polo_prop ["A"; "B"; "=>"] retourne A => B *)

val prop_root : t -> string
(* Récupère le nom du type de noeud à la racine de la proposition
 Argument: la proposition
 Retourne: Name | True | False | Implies | And | Or (strings)*)

val prop_depth : t -> int
(* Retourne la profondeur maximum d'une proposition *)

val prop_items : t -> int
(* Retourne le nombre d'objets (noeuds et feuilles) constituant une proposition *)
