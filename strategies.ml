(* Strategies.ml *)

(* Définitions de types *)
open Hypothese;;

type proposition = Name of string
  | Implies of proposition * proposition
  | True
  | False
  | And of proposition * proposition
  | Or of proposition * proposition;;


type proof = {
    hypos: hypothesis list;
    remainder: proposition list;
  };;

(* Abstract de constructeurs *)
let empty_proof = {hypos=[]; remainder=[]};;

let p_true = True;;

let p_false = False;;

let ( => ) = fun a b -> Implies(a, b);;

let p_name = fun a -> Name(a);;

let ( ^ ) = fun a b -> And(a, b);;

let ( $ ) = fun a b -> Or(a, b);;

let p_not = fun a -> Implies(a, p_false);;

exception Invalid_Input;;
(* Makers de type, à partir de listes de string*)
let make_prop = fun strlist->
  let rec iter_loc = fun list acc ->
    match list with
      ""::rest -> iter_loc rest acc
    | "=>"::rest ->
        begin
          match acc with
            second::ac when ac != [] ->
              begin
                match ac with
                  first::a ->
                    iter_loc rest (Implies(first, second)::a)
                | _ -> Printf.printf "e";raise Invalid_Input
              end
          | _ -> Printf.printf "f";raise Invalid_Input
        end
    | "^"::rest ->
        begin
          match acc with
            second::ac when ac != [] ->
              begin
                match ac with
                  first::a ->
                    iter_loc rest (And(first, second)::a)
                | _ -> Printf.printf "c";raise Invalid_Input
              end
          | _ -> Printf.printf "d";raise Invalid_Input
        end
    | "v"::rest ->
        begin
          match acc with
            second::ac when ac != [] ->
              begin
                match ac with
                  first::a ->
                    iter_loc rest (Or(first, second)::a)
                | _ -> Printf.printf "a";raise Invalid_Input
              end
          | _ -> Printf.printf "b";raise Invalid_Input
        end
    | "Not"::rest ->
        begin
          match acc with
            thing::ac ->
              iter_loc rest (Implies(thing, False)::ac)
          | _ -> raise Invalid_Input
        end
    | "True"::rest ->
        iter_loc rest (True::acc)
    | "False"::rest ->
        iter_loc rest (False::acc)
    | a::rest when a != ""->
        let newacc = Name(a)::acc in
        iter_loc rest newacc
    | _::rest ->
        iter_loc rest acc
    | [] ->
        begin
          match acc with
            elt::rest -> elt
          | _ -> Printf.printf "g";raise Invalid_Input
        end in
  iter_loc strlist [];;

let getAllHypoIds = fun proof ->
  List.map (fun hypo -> hypo.id) proof.hypos;;


(* nextHypId : proof -> int *)
let nextHypId = fun proo ->
    (* Fonction privée qui donne un numéro libre pour une hypothèse *)
    if (getAllHypoIds proo != []) then
      (List.fold_left (fun x y -> max x y) 0 (getAllHypoIds proo))+1
    else 0;;

let add_hyp = fun prp proof ->
   let nexthyp = {id=(nextHypId proof); prop=prp} in
   {hypos=(nexthyp::proof.hypos); remainder=proof.remainder};;

let add_remainder = fun prp proof ->
   {hypos=proof.hypos; remainder=prp::proof.remainder};;

(* Fonctions méthodes sur les types définis plus haut *)

let getRootOfProp = fun prop ->
  match prop with
    Implies(_, _) -> "Implies"
  | And(_, _) -> "And"
  | Or (_, _) -> "Or"
  | True -> "True"
  | False -> "False"
  | _ -> "Other";;

let getProfOfProp = fun prop ->
  let rec iterateurLocal = fun prop niveau ->
    match prop with 
      True | False | Name (_) -> niveau
    | And (p1,p2) | Or (p1,p2) | Implies (p1, p2) -> max (iterateurLocal p1 niveau+1) (iterateurLocal p2 niveau+1) in
  iterateurLocal prop 1;;

let remainderLines = fun proof ->
  List.length proof.remainder;;

let getFirstRemainder = fun proof ->
  List.hd proof.remainder;;

let isProven = fun proof ->
  (proof.remainder = []);;

let getPropOfHyp = fun hypoId proof ->
  (List.find (fun hyp -> hyp.id = hypoId) proof.hypos).prop;;

let getHypList = fun proof->
  proof.hypos;;

let getRemainder = fun proof->
  proof.remainder;;

(* estCeLaBonneHypothese : int -> hypo -> bool*)
let estCeLaBonneHypothese = fun hypoId hypo ->
  (* Fonction privée sensée être utilisée exclusivement dans ce module *)
  getId hypo = hypoId;;

(* Stratégies à appliquer *)

(* andsplit: proof -> bool * proof = <fun> *)
let andsplit = fun proof ->
  (* Fonction qui découpe la première proposition du remainder en deux propositions
  si il s'agit d'un And*)
  match List.hd proof.remainder with
    And(a, b) ->
      let newremainder = a :: (b::List.tl proof.remainder) in
      (true, {hypos=proof.hypos; remainder=newremainder})
  | _ -> (false, proof);;

(* andSplitHypo : int -> proof -> bool*proof = <fun> *)
let andSplitHypo = fun hypoId proof ->
  (* Fonction qui découpe l'hypothèse hypoId en deux hypothèses si il s'agit d'un And*)
  let rec iterateurLocal = fun listeHyposAVider listeHypoARemplir result ->
    match listeHyposAVider with
      [] -> (listeHypoARemplir, result)
    | hypo :: suite ->
        if hypo.id = hypoId
          then match (getProp hypo) with
            And (prop1,prop2) -> iterateurLocal suite ({id=(getId hypo);prop=prop1}::{id=(nextHypId proof);prop=prop2}::listeHypoARemplir) true
          | _ -> iterateurLocal suite (hypo::listeHypoARemplir) false
        else iterateurLocal suite (hypo::listeHypoARemplir) (result||false) in
  let (newListHippos, result) = iterateurLocal proof.hypos [] false in
  (result, {hypos = newListHippos; remainder = proof.remainder})

(* orSplit : bool -> proof -> bool*proof = <fun> *)
let orSplit = fun dejaPasse proof->
  (* Fonction qui découpe le problème en deux sous problèmes si la 1ère proposition du remainder
  est un Or
  dejaPasse sert à indiquer quelle partie du Or fait partie de la preuve.*)
  match proof.remainder with
    prop :: reste ->
      begin
      match prop with
        Or (p1, p2) ->
          if dejaPasse
            then (true, {hypos = proof.hypos; remainder= p2::reste})
            else (true, {hypos = proof.hypos; remainder = p1::reste})
      | _ -> (false, proof)
      end
  | [] -> (false, proof)

(* orSplit : bool int proof -> bool*proof *)
let orSplitHypo = fun dejaPasse idHypo proof->
  (*Fonction qui découpe l'hypothèse idHypo en deux partie si sa proposition est un Or.
  dejaPasse sert à préciser quelle partie du Or est considérée dans la preuve.*)
  let rec iterateurLocal =fun listeHyposAVider listeHypoARemplir result ->
    match listeHyposAVider with
      []->(result,listeHypoARemplir)
    | hypot::reste ->
        if (getId hypot) = idHypo then
          begin
          match (getProp hypot) with
            Or (p1, p2) ->
              if  dejaPasse then iterateurLocal reste ({id=(nextHypId proof);prop=p2}::listeHypoARemplir) true
              else iterateurLocal reste ({id=(nextHypId proof);prop=p1}::listeHypoARemplir) true
          | _ -> iterateurLocal reste (hypot::listeHypoARemplir) (result||false)
          end
        else iterateurLocal reste (hypot::listeHypoARemplir) (result||false) in
  let (rem, res) = iterateurLocal proof.hypos [] false in
  (rem, {hypos=res;remainder=proof.remainder})

(* falseHypo: int proof -> bool * proof *)
(* Si Faux est une hypothèse, on peut tout prouver *)
let falseHypo = fun id proof->
  let propHypo = (List.find (estCeLaBonneHypothese id) proof.hypos).prop in
  if propHypo = False then
    (true, {hypos = proof.hypos; remainder = []})
  else
    (false, proof);;

(* intro : proof -> bool * proof = <fun> *)
let intro = fun proo ->
  match List.hd proo.remainder with
    Implies(True, _)
  | Implies(False, _) -> (false, proo)
  | Implies(a, b) ->
      let nexthyp = {id=(nextHypId proo); prop=a} in
      let nextremainder = b::(List.tl proo.remainder) in
      let newproo = {hypos=(nexthyp::proo.hypos); remainder=nextremainder} in
      (true, newproo)
  | _ -> (false, proo);;

(* estCeQueLHypotheseEstDansLaListe : prop -> hypo -> bool = <fun> *)
let estCeQueLHypotheseEstDansLaListe = fun hypoProp hypo ->
  (* Fonction privée sensée être appelée exclusivement par nettoyer *)
  getProp hypo = hypoProp;;

(* nettoyer: proof -> proof *)
(* Une fonction qui normalise un état de preuve *)
let nettoyer = fun preuve->
  (* Suppression des True du but et des lignes en double *)
  let rec iterLocRemai = fun nettList proprList->
    match nettList with
      True::[] -> iterLocRemai [] proprList
    | True::rest -> iterLocRemai rest proprList
    | prop::rest ->
        if List.mem prop proprList then
          iterLocRemai rest proprList
        else
          iterLocRemai rest (prop::proprList)
    | [] -> proprList in
  (* Tri des propositions dans ordre standard *)
  let newremainder = List.sort compare (iterLocRemai preuve.remainder []) in
  (* Suppression des doublons et des True dans les hypothèses*)
  let rec iterLocHypos = fun nettList proprList->
    match nettList with
      [] -> proprList
    | hyp::rest when (getProp hypo) = True ->
        iterLocHypos rest proprList
    | hyp::rest ->
        if List.exists (estCeQueLHypotheseEstDansLaListe (getProp hypo)) proprList then
          iterLocHypos rest proprList
        else
          iterLocHypos rest (hyp::proprList) in
  let newHypList = List.sort (fun x y -> compare (getProp x) (getProp y)) (iterLocHypos preuve.hypos []) in
  (* renumérotation des hypothèses *)
  let rec renumerotation = fun inacc outacc num->
    match inacc with
      [] -> outacc
    | hyp::rest ->
        renumerotation rest ((newHypo num (getProp hyp))::outacc) (num+1) in
  let renumHypList = renumerotation newHypList [] 0 in
  {hypos=renumHypList; remainder=newremainder};;

(* exact : int -> proof -> bool * proof = <fun> *)
let exact = fun hypoId preuve ->
  (* Verifie si la proposition à prouver est l'hypothèse hypoId *)
  let rec iterateurLocal = fun listeResteAProuver listeNonProuvee result ->
    match listeResteAProuver with
      propos::reste ->
        if (propos = getProp (List.find (estCeLaBonneHypothese hypoId) preuve.hypos))
          then iterateurLocal reste (True :: listeNonProuvee) (result || true)
          else iterateurLocal reste (propos :: listeNonProuvee) (result || false)
    | [] -> let nouvellePreuve = {hypos=preuve.hypos ; remainder = listeNonProuvee}in
        (result,nouvellePreuve) in
  iterateurLocal preuve.remainder [] false;;

(* assumption : proof -> bool * proof = <fun> *)
let assumption = fun preuve ->
  (* Vérifie si la proposition à prouver n'est pas présente dans la liste des hypothèses. *)
  let rec iterateurLocal = fun listeHypothese preuveInterne result->
    match listeHypothese with
      [] -> (result, preuveInterne)
    | hypot :: reste ->
        let numeroHypothese = (getId hypot) in
        let (cond, nouvellePreuve) = exact numeroHypothese preuveInterne in
        iterateurLocal reste nouvellePreuve (cond||result) in
  iterateurLocal preuve.hypos preuve false;;

(* apply: int -> proof -> bool*proof = <fun> *)
let apply = fun hypoId proof ->
  (* Fonction qui applique l'hypothèse selectionée par hypoId à la proposition à prouver *)
  let propHippo = getProp (List.find (estCeLaBonneHypothese hypoId) proof.hypos) in
  match propHippo with
    Implies (partie1,partie2) ->
      if (partie2 = List.hd proof.remainder)
        then (true, {hypos=proof.hypos ; remainder = [partie1]})
        else (false, proof)
  | _ -> (false, proof);;

(* applyInHyp : bool -> int -> int -> proof -> bool*proof = <fun> *)
let applyInHyp = fun keep hypTargetId hypAAppId proof ->
  (* Fonction qui applique l'hypothèse n° hypAAppId dans l'hypothèse n° hypTargetId (si c'est faisable) *)
  let propAAppliquer = (List.find (estCeLaBonneHypothese hypAAppId) proof.hypos).prop in
  let rec iterateurLocal =  fun propToMatch propToReplace listeAVider listeARemplir aBouge ->
    match listeAVider with 
      [] -> (listeARemplir, aBouge)
    | hypo ::reste -> if hypo.id = hypTargetId && hypo.prop = propToMatch
        then if keep 
          then iterateurLocal propToMatch propToReplace reste (hypo:: {id=(nextHypId proof);prop=propToReplace}::listeARemplir) (aBouge||true)
          else iterateurLocal propToMatch propToReplace reste ({id=(nextHypId proof);prop=propToReplace}::listeARemplir) (aBouge||true)
        else iterateurLocal propToMatch propToReplace reste (hypo::listeARemplir) aBouge in 
  match propAAppliquer with 
    Implies (part1, part2) -> let (newHypos, result) = iterateurLocal part1 part2 proof.hypos [] false in
    (result, {hypos = newHypos;remainder = proof.remainder})
  | _-> (false, proof);; 

let prop_iter = fun c_n c_t c_f f_imply f_and f_or prop ->
  let rec iter_local = fun p ->
    match p with
      |Name n -> c_n n
      |True -> c_t
      |False -> c_f
      |Implies (p1,p2) -> f_imply (iter_local p1) (iter_local p2)
      |And (p1,p2) -> f_and (iter_local p1) (iter_local p2)
      |Or (p1, p2) -> f_or (iter_local p1) (iter_local p2) in
  iter_local prop;;

let foncgen_hypo = fun f_id f_prop hypo ->
  (f_id hypo.id, f_prop hypo.prop);;
