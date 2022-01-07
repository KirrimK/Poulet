(* FileIO.ml *)
open Cli;;

let writeInFile = fun nomFic preuve ->
  let listeHypothese = get_hyps preuve in
  let listeButs = get_goal preuve in
  let rec transcrireProp = fun boolHyp propoListe strListe ->
    match propoListe with
      [] -> strListe
    | propo ::reste -> if boolHyp 
        then transcrireProp boolHyp reste ((Printf.sprintf "h: %S" (prop_to_string propo))::strListe)
        else transcrireProp boolHyp reste ((Printf.sprintf "g: %S" (prop_to_string propo))::strListe) in
  let listeDeButsAEcrire = transcrireProp fasle listeButs [] in
  let listeDeTrucsAEcrire = transcrireProp true listeButs listeDeButsAEcrire in
  let rec ecrireChaines = fun listeChaines oc-> 
    match listeChaines with
      [] -> ()
    | ligne::reste -> Printf.fprintf oc "%s\n" ligne;ecrireChaines reste oc in
  let oc = open_out nomFic in ecrireChaines listeDeTrucsAEcrire oc;close_out oc


let load_from_file = fun name ->
  let ic = 
