(* Backtrack.ml *)

open Strategies;;
open Proposition;;
open Proof;;

(* Génération des stratégies applicables pour une état de la preuve donné *)
let getStratList = fun proof hpf ->
  if get_goal proof = [] then (* Aucun but, donc le théorème est déjà prouvé *)
    []
  else
    let first_goal = get_first_goal proof in
    let hyps = get_hyps proof in

    let forAllApplicable = fun other_goals cond cond_prio funcname func ls ->
      let rec gen_lists = fun id ls acc_prio acc_norm->
        match ls with
          a::rest ->
            if cond_prio a then
              gen_lists (id+1) rest ((func id, String.concat " " [funcname; hpf a])::acc_prio) acc_norm
            else if cond a then
              gen_lists (id+1) rest acc_prio ((func id, String.concat " " [funcname; hpf a])::acc_norm)
            else
              gen_lists (id+1) rest acc_prio acc_norm
        | [] -> (acc_prio, acc_norm) in
      gen_lists (if other_goals then 1 else 0) ls [] [] in
    
    let forAllApplicableHypos = forAllApplicable false in
    
    let getOneHypThatIsApplicable = fun cond funcname func hyplist ->
      let rec get_elt = fun id hyplist->
        match hyplist with
          a::rest ->
            if cond a then
              [(func id, String.concat " " [funcname; hpf a])]
            else
              get_elt (id+1) rest
        | [] -> [] in
      get_elt 0 hyplist in

    let tple_list_to_list_tple = fun tple_list->
      let rec tltlt_rec = fun ls acc_left acc_right ->
        match ls with
          (a, b)::rest -> tltlt_rec rest (a::acc_left) (b::acc_right)
        | [] -> (acc_left, acc_right) in
      tltlt_rec tple_list [] [] in
    
    let typeok = (fun _ _->true) in
    let rootIsImplies = p_matchimpl typeok false first_goal in
    let rootIsAnd = p_matchand typeok false first_goal in
    let rootIsOr = p_matchor typeok false first_goal in

    (* Liste des stratégies ne dépendant que du but*)
    let (std_gst_ls, not_prio_gst_ls) =
      if rootIsImplies then
        ([(intro, "intro")], [])
      else if rootIsAnd then
        ([(split, "split")], [])
      else if rootIsOr then (*Si possible, éviter d'avoir à prouver "faux" *)
        if p_matchor (fun x _ -> p_matchfalse true false x) false first_goal then (* il y a un faux à gauche *)
          ([(right, "right")], [(left, "left")])
        else if p_matchor (fun _ y -> p_matchfalse true false y) false first_goal then (* il y a un faux à droit *)
          ([(left, "left")], [(right, "right")])
        else ([(left, "left"); (right, "right")], [])
      else
        ([], []) in

    (* Liste des stratégies prenant des hypothèses en paramètres *)
    
    let (prio_hsplit_ls, std_hsplit_ls) = forAllApplicableHypos (* Récupération des hypothèses traitables par hyp_split*)
        (fun h -> p_matchand (fun x y-> (p_matchfalse true false x || p_matchfalse true false y || x = first_goal || y = first_goal)) false h) (*Cas prioritaire: racine And & contient un false (permettrait de faire un false_hyp juste après, ce qui terminerait la preuve ou but dans le et*)
        (fun h -> p_matchand typeok false h) (*Cas standard: racine And*)
        "hyp_split" hyp_split hyps in

    let (prio_hleft_ls, std_hleft_ls) = forAllApplicableHypos (*Récupération des hypothèses traitables par hyp_left*)
        (fun h -> p_matchor (fun x _-> (p_matchfalse true false x || x = first_goal)) false h) (*Cas prioritaire: racine Or & p_false à gauche ou but à prouver à gauche*)
        (fun h -> p_matchor typeok false h) (*Cas standard: racine Or*)
        "hyp_left" hyp_left hyps in
    
    let (prio_hright_ls, std_hright_ls) = forAllApplicableHypos (*Récupération des hypothèses traitables par hyp_right*)
        (fun h -> p_matchor (fun _ y-> (p_matchfalse true false y || y = first_goal)) false h) (*Cas prioritaire: racine Or & p_false à droite ou but à droite*)
        (fun h -> p_matchor typeok false h) (*Cas standard: racine Or*)
        "hyp_right" hyp_right hyps in

    let tple_ls = List.mapi 
        (fun id a -> forAllApplicableHypos (*Récupération des combinaisons d'hyps qui peuvent s'appliquer entre elles*)
            (fun h -> p_matchimpl (fun x y -> (x = a && (p_matchfalse true false y || List.mem y (get_goal proof)))) false h) (*Cas prioritaire: la partie à gauche de l'implication correspond à l'hypothèse cible, et va générer un false ou un des buts actuellement recherchés*)
            (fun h -> p_matchimpl (fun x _ -> (x = a)) false h) (*Cas standard: la partie à gauche de l'=> correspond à l'hyp cible*)
            (String.concat "" ["applyhyp "; hpf a; " <-"]) (applyInHyp false id) hyps)
        hyps in
    let (prio_apphyp_ls_ls, std_apphyp_ls_ls) = tple_list_to_list_tple tple_ls in
    let prio_apphyp_ls = List.concat prio_apphyp_ls_ls in
    let std_apphyp_ls = List.concat std_apphyp_ls_ls in
    
    let (prio_apply_ls, std_apply_ls) = forAllApplicableHypos (*Récupération des hypothèses applicables au but via apply*)
        (fun h -> p_matchimpl (fun x y -> (y = first_goal && List.mem x hyps)) false h) (*Cas prioritaire: la stratégie est applicable et ce qu'il en résultera est dans les hypothèses*)
        (fun h -> p_matchimpl (fun _ y -> y = first_goal) false h) (*Cas standard: la stratégie est applicable*)
        "apply" apply hyps in

    let exact_list = getOneHypThatIsApplicable (*Récupération de l'hypothèse qui est exactement le but: très prioritaire *)
        (fun h -> h = first_goal)
        "exact" exact hyps in

    let false_hyp_list = getOneHypThatIsApplicable (*Récupération de l'hypothèse qui est fausse, et qui terminera la preuve *)
        (fun h -> h = p_false)
        "hyps has" false_hyp hyps in
        
    (* Agrégation des listes, avec les stratégies prioritaires en premier *)
    (* Ordre: hypothèse fausse, exact, les stratégies sur le but (souvent utilisées), ce qui génère des faux dans les hyps,  ce qui génère des hyps exact-ables, le reste *)
    List.concat [false_hyp_list; exact_list; prio_hsplit_ls; prio_hleft_ls; prio_hright_ls; std_gst_ls; prio_apphyp_ls; prio_apply_ls; std_apphyp_ls; std_apply_ls; std_hsplit_ls; std_hleft_ls; std_hright_ls; not_prio_gst_ls];;

(* Algorithme du backtrack *)
type state = {visited: Proof.t list; backnum: int; depth: int};;

let backtrack = fun prints hpf proof->
  let rec backrec = fun norm_proo nameacc stateacc->
    (* Vérifier appartenance à la liste des états déjà visités *)
    let () = if (prints < 2) then (Printf.printf "\r-> %d|%d  %!" stateacc.backnum stateacc.depth) else () in
    if List.mem norm_proo (stateacc.visited) then (* L'état a déjà été visité *)
      let () = if prints = 2 then Printf.printf "%s| Already visited.\n" (String.make stateacc.depth '-') (*nameacc*) else () in
      ((false, norm_proo), stateacc)
    else (* L'état n'a jamais été visité *)
      begin
        (* Ajouter l'état à la liste des états visités *)
        let newstateacc = {visited=(norm_proo::(stateacc.visited)); backnum=stateacc.backnum; depth=(stateacc.depth+1)} in
        let stratList = getStratList norm_proo hpf in (* Récupérer la liste des stratégies applicables à ce stade *)
        (* Explorer toutes les stratégies dans la liste *)
        let rec explore = fun stratlist recstateacc->
          match stratlist with
            (strat, stratname)::rest -> (* Encore des stratégies à tester *)
              let (result, resproof) = strat norm_proo in (* Tester la stratégie *)
              let norm_resproof = clean resproof in (* Nettoyer et normaliser *)
              let newnameacc = String.concat (if prints = 2 then " > " else "\n> ") [nameacc;stratname] in
              if result then (* La stratégie à fait progresser la preuve*)
                if is_proven norm_resproof then (* Est-ce que la preuve est finie *)
                  let () = if prints > 0 then  Printf.printf "%s%s (Proven)\n%s | Proof done\n" (String.make stateacc.depth '-') stratname newnameacc else () in
                  ((true, norm_resproof), recstateacc)
                else (* La preuve n'est pas encore finie, explorer le nouveau noeud de l'arbre*)
                  let () = if prints = 2 then (Printf.printf "%s%s (progress)\n" (String.make stateacc.depth '-') stratname) else () in
                  let backresult = backrec norm_resproof newnameacc recstateacc in
                  match backresult with
                    ((true, backres), state) -> ((true, backres), state) (* Le backtrack a réussi à prouver *)
                  | ((false, _), state)  -> explore rest state (* Essayer les autres possibilités *)
              else (* La stratégie à échoué *)
                let () = if prints = 2 then Printf.printf "%s%s (fail)" (String.make stateacc.depth '-') stratname else () in
                explore rest {visited=(recstateacc.visited); backnum=(recstateacc.backnum + 1); depth=(recstateacc.depth)} (* Essayer le reste des stratégies à ce niveau *)
          | [] -> (* Plus de stratégies à tester à ce stage *)
              if prints = 2 then
                (Printf.printf "%s| No more applicable strategies.\n" (String.make stateacc.depth '-') (*nameacc*));
              ((false, norm_proo), {visited=recstateacc.visited; backnum=(recstateacc.backnum + 1); depth=(recstateacc.depth - 1)}) in
        explore stratList newstateacc
      end in
  let t_start = Sys.time () in
  let ((res, proof), state) = backrec (clean proof) (if prints = 2 then "\nbacktrack" else "\rbacktrack") {visited=[]; backnum=0; depth=0} in
  let () = Printf.printf "\rDone %d backtracks, depth achieved %d. Took %fsecs.\n" state.backnum state.depth (Sys.time () -. t_start) in
  (res, proof);;
