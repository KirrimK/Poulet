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
  let (listesHypotheses,listeButs) = demake_proof preuve in
  let listeIndices = goal_ids preuve in
  let rec transcrireGoals = fun propoListe strListe nbPreuveListe->
    match propoListe with
      [] -> strListe
    | propo ::reste -> 
        transcrireGoals reste ((Printf.sprintf "g %d: %s" (List.hd nbPreuveListe) (propToString propo))::strListe) (List.tl nbPreuveListe) in
  let rec transcrireHypListe = fun listeHyp strListe nbPreuveEnCours->
    match listeHyp with
      []-> strListe
    | propo::reste -> 
        transcrireHypListe reste (Printf.sprintf "h %d: %s" nbPreuveEnCours (propToString propo)::strListe) nbPreuveEnCours in
  let rec transcrireHyps = fun listesHyps strListe nbPreuveEnCours ->
    match listesHyps with
      [] -> strListe
    | listeHypo ::reste -> transcrireHyps reste (transcrireHypListe listeHypo strListe nbPreuveEnCours) (nbPreuveEnCours+1)in
  let listeDeButsAEcrire = transcrireGoals listeButs [] listeIndices in
  let listeDeTrucsAEcrire = transcrireHyps listesHypotheses listeDeButsAEcrire 0 in
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
          readLines ((-1,strProp2)::accH) accG
      |  "g"::strProp -> 
          let strProp2 = String.concat "" [String.concat " " strProp;"\n"] in
          readLines accH ((-1,strProp2)::accG)
      | debut::strProp -> let debutCoupe = String.split_on_char ' ' debut in
          begin
            match debutCoupe with
              "h"::strId ->
                let strProp2 = String.concat "" [String.concat " " strProp;"\n"] in
                readLines (((int_of_string (String.concat "" strId)),strProp2)::accH) accG
            | "g"::strId ->
                let strProp2 = String.concat "" [String.concat " " strProp;"\n"] in
                readLines accH (((int_of_string (String.concat "" strId)),strProp2)::accG)
            | _ -> readLines accH accG
          end
      | _ -> readLines accH accG
    with End_of_file -> (accH, accG) in
  let (listeHyp, listeBut) = readLines [] [] in
  close_in ic;
  let getFirstElt = fun t -> match t with (elt,_)->elt in
  let compTuple = fun t1 t2 -> (getFirstElt t1)- (getFirstElt t2) in
  let listeHypTriee = List.sort compTuple listeHyp in
  let listeButTriee = List.sort compTuple listeBut in
  let rec createAs = fun aListe listeButTriee ->
    match listeButTriee with
      [] -> List.rev aListe
    | (id,butStr)::reste-> 
        let lexbuf = Lexing.from_string butStr in Printf.printf "%d\n"(id);
        let but = Parser.main Lexer.token lexbuf in
        createAs ((make_a [] but)::aListe) reste in
  let rec fillAs = fun aListeARemplir aListeRemplie hypListe countProofs ->
    match aListeARemplir with
      [] -> aListeRemplie
    | a::reste-> 
        begin
          match hypListe with
            []-> List.concat [aListeARemplir;aListeRemplie]
          | (id,strProp)::suite ->
              if id = countProofs
                then 
                  let lexbuf = Lexing.from_string strProp in
                  let propo = Parser.main Lexer.token lexbuf in
                  fillAs ((add_hyp_to_a propo a)::reste) aListeRemplie suite countProofs
                else
                  fillAs reste (a::aListeRemplie) hypListe (countProofs+1)
        end in
  let listeAs = createAs [] listeButTriee in
  Printf.printf "nb BUTS : %d\n" (List.length listeAs);
  fillAs listeAs [] listeHypTriee 0;;
