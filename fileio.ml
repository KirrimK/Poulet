(* FileIO.ml *)
open Proof;;
open Proposition;;

let cName = fun n -> n ;;
let cTrue = "TRUE";;
let cFalse = "FALSE" ;;

let fImplies = fun s1 s2 -> String.concat "" ["(";s1;" => ";s2;")"];;

let fAnd = fun sProp1 sProp2 -> String.concat "" ["(";sProp1;" ^ ";sProp2;")"];;

let fOr = fun sProp1 sProp2 -> String.concat "" ["(";sProp1;" | ";sProp2;")"];;

let propToString = fun propo ->
  prop_iter cName cTrue cFalse fImplies fAnd fOr propo;;

let writeInFile = fun nomFic preuve ->
  let listeHypothese = get_hyps preuve in
  let listeButs = get_goal preuve in
  let listeIndices = goal_ids preuve in
  let rec transcrireProp = fun boolHyp propoListe strListe nbPreuveListe->
    match propoListe with
      [] -> strListe
    | propo ::reste -> 
        transcrireProp boolHyp reste ((Printf.sprintf (if boolHyp then "h%d: %s" else "g%d: %s") (List.hd nbPreuveListe) (propToString propo))::strListe) (List.tl nbPreuveListe)
  let listeDeButsAEcrire = transcrireProp false listeButs [] listeIndices in
  let listeDeTrucsAEcrire = transcrireProp true listeHypothese listeDeButsAEcrire in
  let rec ecrireChaines = fun listeChaines oc->
    match listeChaines with
      [] -> ()
    | ligne::reste -> Printf.fprintf oc "%s\n" ligne;ecrireChaines reste oc in
  let oc = open_out nomFic in ecrireChaines listeDeTrucsAEcrire oc;close_out oc


let load_from_file = fun name ->
  let ic = open_in name in
  let rec readLines = fun accH accG ->
    try
      let ligne = input_line ic in
      let ligneCoupee = String.split_on_char ':' ligne in
      match ligneCoupee with
        "h"::strProp ->
          let strProp2 = String.concat "" [String.concat " " strProp;"\n"] in
          readLines (strProp2::accH) accG
      | "g"::strProp ->
          let strProp2 = String.concat "" [String.concat " " strProp;"\n"] in
          readLines accH (strProp2::accG)
      | _ -> readLines accH accG
    with End_of_file -> (accH, accG) in
  let (listeHyp, listeBut) = readLines [] [] in
  close_in ic;
  let rec addProps = fun boolHyp liste preuve ->
    match liste with
      [] -> preuve
    | stringProp :: reste ->
      let lexbuf = Lexing.from_string stringProp in
      let propo = Parser.main Lexer.token lexbuf in
      let nouvPreuve = (if boolHyp then add_hyp else add_goal) propo preuve in
      addProps boolHyp reste nouvPreuve in
  let preuve = addProps false listeBut empty in
  let hyp_on_first = addProps true listeHyp preuve in
  List.map (fun x-> make_a (get_hyps hyp_on_first) x) (get_goal hyp_on_first);;
