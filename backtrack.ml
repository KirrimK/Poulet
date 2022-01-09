(* Backtrack.ml *)

open Strategies;;
open Proposition;;
open Proof;;


(* let hpf_basic = fun id _ ->
  string_of_int id;; *)



(* Génération des stratégies applicables pour une état de la preuve donné *)
let getStratList = fun proof hpf ->
  (* Fonction locale qui génère une liste de stratégies appliquables sur les hypothèses, et ne propose leur application que si la strategie est compatible avec l'hypothèse *)
  if get_goal proof = [] then
    []
  else
    let hypIds = hyp_ids proof in
    let otherGoalIds =
      (match goal_ids proof with
        _::rest -> rest
      | [] -> []) in
    let forAllApplicableHypos = fun predicat func funcname hypoIdsList ->
      List.map (fun id -> (func id, String.concat " " [funcname; hpf id proof])) (List.filter predicat hypoIdsList) in

    let forAllGoals = fun func funcname goalidlist->
      List.map (fun id -> (func id, String.concat " " [funcname; string_of_int id])) goalidlist in

    let addStratToList = fun predicat stratandstratname stratlist ->
      if predicat then
        stratandstratname::stratlist
      else
        stratlist in
    let rootIsImplies = (prop_root (get_first_goal proof) = "Implies") in
    let rootIsAnd = (prop_root (get_first_goal proof) = "And") in
    let rootIsOr = (prop_root (get_first_goal proof) = "Or") in

    (* Liste des stratégies ne dépendant que du but*)
    let goalStratlist =
      addStratToList rootIsImplies (intro, "intro")
        (addStratToList rootIsAnd (split, "split")
           (addStratToList rootIsOr (left, "hyp_left")
              (addStratToList rootIsOr (right, "hyp_right") []))) in

    (* Liste des stratégies visant à changer de but à prouver *)
    let switch_goal_list = forAllGoals select_goal "selected" otherGoalIds in

    (* Liste des stratégies prenant des hypothèses en paramètres *)
    (* Séparation d'une hypothèse "And" en deux *)
    let andSplitHypList = forAllApplicableHypos (fun x -> prop_root (get_hyp x proof) = "And") hyp_split "hyp_split" hypIds in

    (* Séparation d'une hypothèse "Or" en deux sous-pbs *)
    let orSplitHypLeftList = forAllApplicableHypos (fun x -> prop_root (get_hyp x proof) = "Or") hyp_left "hyp_left" hypIds in
    let orSplitHypRightList = forAllApplicableHypos (fun x -> prop_root (get_hyp x proof) = "Or") hyp_right "hyp_right" hypIds in

    (* Application d'une hypothèse à une autre
       Ne pas utiliser si le applyhypo crée de nouvelles hypothèses plutot que modifier*)
    (* Hyp à modifier en premier, Hyp à appliquer en seconde *)
    let applyHypList = List.concat (List.map (fun a ->
        forAllApplicableHypos (fun x->p_matchimpl (fun x _ -> x = (get_hyp a proof)) false (get_hyp x proof)) (applyInHyp false a) (String.concat "" ["applyhyp "; hpf a proof; " <-"]) hypIds) hypIds) in

    (* Application d'une hypothèse au but *)
    let applyList = forAllApplicableHypos (fun x->p_matchimpl (fun _ y-> y = (get_first_goal proof)) false (get_hyp x proof)) apply "apply" hypIds in

    (* Exacts des hypothèses au but *)
    let exactList = forAllApplicableHypos (fun x-> (get_hyp x proof) = get_first_goal proof) exact "exact" (hyp_ids proof) in

    (* Terminaison de la preuve si une hypothèse est "Faux" *)
    let falseHypList = forAllApplicableHypos (fun x -> (get_hyp x proof) = p_false) false_hyp "hypos has" hypIds in

    (* Agrégation des listes *)
    List.concat [falseHypList; exactList; goalStratlist; applyList; orSplitHypLeftList; orSplitHypRightList; andSplitHypList; applyHypList; switch_goal_list];;

(* Algorithme du backtrack *)
type state = {visited: Proof.t list; num: int};;

let backtrack = fun prints hpf proof->
  let rec backrec = fun norm_proo nameacc stateacc->
    (* Vérifier appartenance à la liste des états déjà visités *)
    let () = if (((stateacc.num mod 100) = 0) && prints < 2) then (Printf.printf "\r-> %d%!" stateacc.num) else () in
    if List.mem norm_proo (stateacc.visited) then (* L'état a déjà été visité *)
      let () = if prints = 2 then Printf.printf "%s | Already visited.\n" nameacc else () in
      ((false, norm_proo), stateacc)
    else (* L'état n'a jamais été visité *)
      begin
        (* Ajouter l'état à la liste des états visités *)
        let newstateacc = {visited=(norm_proo::(stateacc.visited)); num=stateacc.num} in
        let stratList = getStratList norm_proo hpf in (* Récupérer la liste des stratégies applicables à ce stade *)
        (* Explorer toutes les stratégies dans la liste *)
        let rec explore = fun stratlist stateacc->
          match stratlist with
            (strat, stratname)::rest -> (* Encore des stratégies à tester *)
              let (result, resproof) = strat norm_proo in (* Tester la stratégie *)
              let norm_resproof = clean resproof in (* Nettoyer et normaliser *)
              let newnameacc = String.concat (if prints = 2 then " > " else "\n> ") [nameacc;stratname] in
              if result then (* La stratégie à fait progresser la preuve*)
                if is_proven norm_resproof then (* Est-ce que la preuve est finie *)
                  let () = if prints > 0 then  Printf.printf "%s | Proof done\n" newnameacc else () in
                  ((true, norm_resproof), stateacc)
                else (* La preuve n'est pas encore finie, explorer le nouveau noeud de l'arbre*)
                  let () = if prints = 2 then (Printf.printf "%s (progress)\n" newnameacc) else () in
                  let backresult = backrec norm_resproof newnameacc stateacc in
                  match backresult with
                    ((true, backres), state) -> ((true, backres), state) (* Le backtrack a réussi à prouver *)
                  | ((false, _), state)  -> explore rest state (* Essayer les autres possibilités *)
              else (* La stratégie à échoué *)
                let () = if prints = 2 then Printf.printf "%s (fail)\n" newnameacc else () in
                explore rest {visited=(stateacc.visited); num=(stateacc.num + 1)} (* Essayer le reste des stratégies à ce niveau *)
          | [] -> (* Plus de stratégies à tester à ce stage *)
              if prints = 2 then
                (Printf.printf "%s | No more applicable strategies.\n" nameacc);
              ((false, norm_proo), {visited=stateacc.visited; num=(stateacc.num + 1)}) in
        explore stratList newstateacc
      end in
  let ((res, proof), state) = backrec (clean proof) (if prints = 2 then "\nbacktrack" else "\rbacktrack") {visited=[]; num=0} in
  let () = Printf.printf "\rDone %d backtracks.\n" state.num in
  (res, proof);;
