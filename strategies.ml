(* Strategies.ml *)

open Proposition;;
open Proof;;

let fail = fun x -> (false, x);;

let intro = fun proof ->
  let failed = fail proof in
  let (goal, rest) = get_goal proof in
  prop_iter (fun x->failed) failed failed (fun x y->
    if x = p_true || x = p_false then
      failed
    else
      (true, make_proof (x::(get_hyps proof)) y::rest)
                                          ) failed failed goal;;

let split = fun proof ->
  let failed = fail proof in
  let (goal, rest) = get_goal proof in
  prop_iter (fun x->failed) failed failed failed (fun x y->
    (true, make_proof (get_hyps proof) (x::(y::rest)))) failed goal;;

let hyp_split = fun id proof ->
  let failed = fail proof in
  let other_hyps = remove_hyp id proof in
  prop_iter (fun x->failed) failed failed failed (fun x y->
    (true, make_proof (x::(y::other_hyps)) (get_goal proof) failed (get_hyp id proof));;

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
        else iterateurLocal suite (hypo::listeHypoARemplir) result in
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
          | _ -> iterateurLocal reste (hypot::listeHypoARemplir) result
          end
        else iterateurLocal reste (hypot::listeHypoARemplir) result in
  let (rem, res) = iterateurLocal proof.hypos [] false in
  (rem, {hypos=res;remainder=proof.remainder})

(* falseHypo: int proof -> bool * proof *)
(* Si Faux est une hypothèse, on peut tout prouver *)
let falseHypo = fun id proof->
  let propHypo = getProp (List.find (estCeLaBonneHypothese id) proof.hypos) in
  if propHypo = False then
    (true, {hypos = proof.hypos; remainder = []})
  else
    (false, proof);;

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
          then iterateurLocal reste (True :: listeNonProuvee) true
          else iterateurLocal reste (propos :: listeNonProuvee) result
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
  let propAAppliquer = getProp (List.find (estCeLaBonneHypothese hypAAppId) proof.hypos) in
  let rec iterateurLocal =  fun propToMatch propToReplace listeAVider listeARemplir aBouge ->
    match listeAVider with 
      [] -> (listeARemplir, aBouge)
    | hypo ::reste -> if (getId hypo) = hypTargetId && (getProp hypo) = propToMatch
        then if keep 
          then iterateurLocal propToMatch propToReplace reste (hypo:: (newHypo (nextHypId proof) propToReplace) ::listeARemplir) true
          else iterateurLocal propToMatch propToReplace reste ((newHypo (nextHypId proof) propToReplace)::listeARemplir) true
        else iterateurLocal propToMatch propToReplace reste (hypo::listeARemplir) aBouge in 
  match propAAppliquer with 
    Implies (part1, part2) -> let (newHypos, result) = iterateurLocal part1 part2 proof.hypos [] false in
    (result, {hypos = newHypos;remainder = proof.remainder})
  | _-> (false, proof);; 
