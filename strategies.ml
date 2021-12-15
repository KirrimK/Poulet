(* Strategies.ml *)

(* Définitions de types *)
type proposition = Name of string
  | Implies of proposition * proposition
  | True
  | False
  | Negation of proposition
  | And of proposition * proposition;;

type hypothesis = {
    id: int;
    prop: proposition;
  };;

type proof = {
    hypos: hypothesis list;
    remainder: proposition list;
  };;

(* Fonctions méthodes sur les types définis plus haut *)

let getAllHypoIds = fun proof ->
  List.map (fun hypo -> hypo.id) proof.hypos;;

let getRootOfProp = fun prop ->
  match prop with
    Implies(_, _) -> "Implies"
  | Negation _ -> "Negation"
  | And(_, _) -> "And"
  | _ -> "Other";;

let remainderLines = fun proof ->
  List.length proof.remainder;;

let getFirstRemainder = fun proof ->
  List.hd proof.remainder;;

let isRemainderTrue = fun proof ->
  (proof.remainder = [True]);;

let splitProblem = fun proof ->
  List.map (fun remline -> {hypos=proof.hypos; remainder=[remline]}) proof.remainder;;

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

(* nextHypId : proof -> int *)
let nextHypId = fun proo ->
    (* Fonction privée qui donne un numéro libre pour une hypothèse *)
    if (getAllHypoIds proo != []) then
      (List.fold_left (fun x y -> max x y) 0 (getAllHypoIds proo))+1
    else 0;; 

(* andSplitHypo : int -> proof -> bool*proof = <fun> *)
let andSplitHypo = fun hypoId proof ->
  (* Fonction qui découpe l'hypothèse hypoId en deux hypothèses si il s'agit d'un And*)
  let rec iterateurLocal = fun listeHyposAVider listeHypoARemplir result ->
    match listeHyposAVider with
      [] -> (listeHypoARemplir, result)
    | hypo :: suite -> 
        if hypo.id = hypoId 
          then match hypo.prop with
            And (prop1,prop2) -> iterateurLocal suite ({id=(nextHypId proof);prop=prop1}::{id=(nextHypId proof);prop=prop2}::listeHypoARemplir) true
          | _ -> iterateurLocal suite (hypo::listeHypoARemplir) false
        else iterateurLocal suite (hypo::listeHypoARemplir) (result||false) in
  let (newListHippos, result) = iterateurLocal proof.hypos [] false in
  (result, {hypos = newListHippos; remainder = proof.remainder})


(* intro : proof -> bool * proof = <fun> *)
let intro = fun proo ->
  match List.hd proo.remainder with
    Implies(True, _) -> (false, proo)
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
  hypo.prop = hypoProp;;

(* nettoyer : proof -> bool* proof *)
let nettoyer = fun preuve ->
  (* fonction qui supprime les doublons dans les hypothèses et enlève les True dans le remainder 
  si il y a des doublons*)
  let rec iterateurLocalReste = fun listeANettoyer listePropre result nbPropLaisses->
    match listeANettoyer with
      True :: [] -> iterateurLocalReste [] listePropre (result || false) nbPropLaisses
    | True :: reste -> iterateurLocalReste reste listePropre (result || true) nbPropLaisses
    | propos :: reste -> iterateurLocalReste reste (propos :: listePropre) (result || false) (nbPropLaisses+1)
    | [] -> 
        if (nbPropLaisses >0) 
          then (result, listePropre) 
          else (result, [True]) in
  let rec iterateurLocalHippo = fun listeANettoyer listePropre result ->
    match listeANettoyer with
      [] -> (result,listePropre)
    | hippo :: reste -> 
      if ((List.find_opt (estCeQueLHypotheseEstDansLaListe hippo.prop) listePropre)=None)
        then iterateurLocalHippo reste (hippo::listePropre) (result || false)
        else iterateurLocalHippo reste listePropre (result||true) in
  let (aMarche1,nouveauResteAProuver) = iterateurLocalReste preuve.remainder [] false 0 in
  let (aMarche2,nouvellesHippos) = iterateurLocalHippo preuve.hypos [] false in
  ((aMarche1||aMarche2), {hypos = nouvellesHippos; remainder = nouveauResteAProuver})

(* estCeLaBonneHypothese : int -> hypo -> bool*)
let estCeLaBonneHypothese = fun hypoId hypo ->
  (* Fonction privée sensée être utilisée exclusivement par exact *)
  hypo.id = hypoId;;

(* exact : int -> proof -> bool * proof = <fun> *)
let exact = fun hypoId preuve ->
  (* Verifie si la proposition à prouver est l'hypothèse hypoId *)
  let rec iterateurLocal = fun listeResteAProuver listeNonProuvee result ->
    match listeResteAProuver with
      propos::reste -> 
        if (propos = (List.find (estCeLaBonneHypothese hypoId) preuve.hypos).prop)
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
        let numeroHypothese = hypot.id in
        let (cond, nouvellePreuve) = exact numeroHypothese preuveInterne in
        iterateurLocal reste nouvellePreuve (cond||result) in
  iterateurLocal preuve.hypos preuve false;;

(* apply: int -> proof -> bool*proof = <fun> *)
let apply = fun hypoId proof ->
  (* Fonction qui applique l'hypothèse selectionée par hypoId à la proposition à prouver *)
  let propHippo = (List.find (estCeLaBonneHypothese hypoId) proof.hypos).prop in
  match propHippo with
    Implies (partie1,partie2) -> 
      if (partie2 = List.hd proof.remainder)
        then (true, {hypos=proof.hypos ; remainder = [partie1]})
        else (false, proof)
  | _ -> (false, proof);;

let prop_iter = fun c_n c_t c_f f_neg f_imply f_and prop ->
  let rec iter_local = fun p ->
    match p with
      |Name n -> c_n n
      |True -> c_t
      |False -> c_f
      |Negation neg -> f_neg (iter_local neg)
      |Implies (p1,p2) -> f_imply (iter_local p1) (iter_local p2)
      |And (p1,p2) -> f_and (iter_local p1) (iter_local p2) in
  iter_local prop;;

let foncgen_hypo = fun f_id f_prop hypo ->
  (f_id hypo.id, f_prop hypo.prop);;

