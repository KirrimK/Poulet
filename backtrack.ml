(* Backtrack.ml *)

open Strategies;;

let hpf_basic = fun id proof ->
  string_of_int id;;

(* Génération des stratégies applicables pour une état de la preuve donné *)
let getStratList = fun proof hpf ->
  (* Fonction locale qui génère une liste de stratégies appliquables sur les hypothèses, et ne propose leur application que si la strategie est compatible avec l'hypothèse *)
  let hypIds = getAllHypoIds proof in
  let forAllApplicableHypos = fun predicat func funcname hypoIdsList ->
    List.map (fun id -> (func id, String.concat " " [funcname; hpf id proof])) (List.filter predicat hypoIdsList) in
  let addStratToList = fun predicat stratandstratname stratlist ->
    if predicat then
      stratandstratname::stratlist
    else
      stratlist in
  let rootIsImplies = (getRootOfProp (getFirstRemainder proof) = "Implies") in
  let rootIsAnd = (getRootOfProp (getFirstRemainder proof) = "And") in
  let rootIsOr = (getRootOfProp (getFirstRemainder proof) = "Or") in

  (* Liste des stratégies ne dépendant que du but*)
  let goalStratlist =
    addStratToList rootIsImplies (intro, "intro")
      (addStratToList rootIsAnd (andsplit, "andSplit")
         (addStratToList rootIsOr (orSplit false, "orSplit-left")
            (addStratToList rootIsOr (orSplit true, "orSplit-right") []))) in

  (* Liste des stratégies prenant des hypothèses en paramètres *)
  (* Séparation d'une hypothèse "And" en deux *)
  let andSplitHypList = forAllApplicableHypos (fun x -> getRootOfProp (getPropOfHyp x proof) = "And") andSplitHypo "andSplitHypo" hypIds in

  (* Séparation d'une hypothèse "Or" en deux sous-pbs *)
  let orSplitHypLeftList = forAllApplicableHypos (fun x -> getRootOfProp (getPropOfHyp x proof) = "Or") (orSplitHypo true) "orSplitHypo-left" hypIds in
  let orSplitHypRightList = forAllApplicableHypos (fun x -> getRootOfProp (getPropOfHyp x proof) = "Or") (orSplitHypo false) "orSplitHypo-right" hypIds in

  (* Application d'une hypothèse à une autre
   Ne pas utiliser si le applyhypo crée de nouvelles hypothèses plutot que modifier*)
  (* Hyp à modifier en premier, Hyp à appliquer en seconde *)
  let applyHypList = List.concat (List.map (fun x -> forAllApplicableHypos (fun x -> getRootOfProp (getPropOfHyp x proof) = "And") (applyInHyp false x) (String.concat "" ["applyInHyp "; hpf x proof]) hypIds) hypIds) in

  (* Application d'une hypothèse au but *)
  let applyList = forAllApplicableHypos (fun x -> true) apply "apply" hypIds in

  (* Exacts des hypothèses au but *)
  let exactList = forAllApplicableHypos (fun x -> true) exact "exact" (getAllHypoIds proof) in

  (* Terminaison de la preuve si une hypothèse est "Faux" *)
  let falseHypList = forAllApplicableHypos (fun x -> getRootOfProp (getPropOfHyp x proof) = "False") falseHypo "hypos has" hypIds in

  (* Agrégation des listes *)
  List.concat [falseHypList; goalStratlist; applyList; exactList; orSplitHypLeftList; orSplitHypRightList; andSplitHypList];;

(* Algorithme du backtrack *)
let backtrack = fun proof prints hpf->
  let stateMemory = ref [] in (* Mémoire de tous les états déjà visités *)
  (* *)
  let rec backrec = fun proo nameacc->
    (* Nettoyer l'objet preuve et normaliser *)
    let norm_proo = nettoyer proo in
    (* Vérifier appartenance à la liste des états déjà visités *)
    if List.mem norm_proo !stateMemory then (* L'état a déjà été visité *)
      let () = if prints then Printf.printf "%s | Déjà visité.\n" nameacc else () in
      (false, proo)
    else (* L'état n'a jamais été visité *)
      begin
        (* Ajouter l'état à la liste des états visités *)
        stateMemory := (norm_proo::!stateMemory);
        let stratList = getStratList norm_proo hpf in (* Récupérer la liste des stratégies applicables à ce stade *)
        (* Explorer toutes les stratégies dans la liste *)
        let rec explore = fun stratlist->
          match stratlist with
            (strat, stratname)::rest -> (* Encore des stratégies à tester *)
              let (result, resproof) = strat norm_proo in (* Tester la stratégie *)
              let norm_resproof = nettoyer resproof in (* Nettoyer et normaliser *)
              let newnameacc = String.concat " > " [nameacc;stratname] in
              if result then (* La stratégie à fait progresser la preuve*)
                if isProven norm_resproof then (* Est-ce que la preuve est finie *)
                  let () = Printf.printf "%s | Preuve finie\n" newnameacc in
                  (true, norm_resproof)
                else (* La preuve n'est pas encore finie, explorer le nouveau noeud de l'arbre*)
                  let () = if prints then (Printf.printf "%s (progrès)\n" newnameacc) else () in
                  let backresult = backrec norm_resproof newnameacc in
                  match backresult with
                    (true, backres) -> (true, backres) (* Le backtrack a réussi à prouver *)
                  | (false, _) -> explore rest (* Essayer les autres possibilités *)
              else (* La stratégie à échoué *)
                let () = if prints then Printf.printf "%s (fail)\n" newnameacc else () in
                explore rest (* Essayer le reste des stratégies à ce niveau *)
          | [] -> (* Plus de stratégies à tester à ce stage *)
              if prints then
                (Printf.printf "%s | Plus de stratégies applicables.\n" nameacc);
              (false, proo) in
        explore stratList
      end in
  backrec proof "backtrack";;
