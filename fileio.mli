(*Fichier qui gère les écritures et lecture dans des fichiers d'objets preuve (extension .hen conseillée) *)

val writeInFile : string -> Proof.t -> unit;;

val load_from_file : string -> Proof.t;;