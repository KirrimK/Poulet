(*Fichier qui gère les écritures et lecture dans des fichiers d'objets preuve (extension .hen conseillée) *)

val writeInFile : string -> Proof.t -> unit;;
(* Permet d'écrire un état de preuve dans un fichier dont le nom est passé en argument *)

val load_from_file : string -> Proof.t;;
(* Lit le fichier dont le nom est passé en argument afin d'en extraire un état de preuve *)
