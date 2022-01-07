(* FileIO.ml *)
open Cli;;
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
  let rec transcrireProp = fun boolHyp propoListe strListe ->
    match propoListe with
      [] -> strListe
    | propo ::reste -> if boolHyp 
        then transcrireProp boolHyp reste ((Printf.sprintf "h: %S" (propToString propo))::strListe)
        else transcrireProp boolHyp reste ((Printf.sprintf "g: %S" (propToString propo))::strListe) in
  let listeDeButsAEcrire = transcrireProp fasle listeButs [] in
  let listeDeTrucsAEcrire = transcrireProp true listeButs listeDeButsAEcrire in
  let rec ecrireChaines = fun listeChaines oc-> 
    match listeChaines with
      [] -> ()
    | ligne::reste -> Printf.fprintf oc "%s\n" ligne;ecrireChaines reste oc in
  let oc = open_out nomFic in ecrireChaines listeDeTrucsAEcrire oc;close_out oc


let load_from_file = fun name ->
  let ic = open_in name in
  let rec readLines = fun accH accG ->
    let ()=
      try
        let ligne = read_line ic in
        let ligneCoupee = String.split ":" ligne in
        match ligneCoupee with
        "h"::strProp -> 
          let strProp2 = String.concat "" [strProp;"\n"] in 
          readLines (strProp2::accH) accG
      | "g"::strProp -> 
          let strProp2 = String.concat "" [strProp;"\n"] in 
          readLines accH (strProp2::accG)
      | _ -> readLines accH accG 
      with End_of_file -> (accH, accG) in () in
  let (listeHyp, listeBut) = readLines [] [] in
  close_in ic;
  let p = empty in
  let rec addProps = fun boolHyp liste preuve ->
    match liste with
      [] -> preuve
    | stringProp :: reste -> 
      let lexbuf = Lexing.from_string stringProp in
      let propo = Parser.main Lexer.token lexbuf in 
      let nouvPreuve = (if boolHyp then add else add_goal) propo preuve in
      addProps boolHyp reste nouvPreuve in
  let preuve = addProps true listeHyp empty in
  addProps false listeBut preuve
