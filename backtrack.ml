(* Backtrack.ml *)

open Strategies;;

let buildfunclist = fun proof ->
  let funclist = fun func funcname hypolist ->
    List.map (fun id -> (func id, String.concat " " [funcname;(string_of_int id)])) hypolist in
  let applylist = funclist (fun x y z -> apply x y) "apply" (getAllHypoIds proof) in
  let exactlist = funclist (fun x y z -> exact x y) "exact" (getAllHypoIds proof) in
  let orsplithypolist = funclist orSplitHypo "orsplithypo" (getAllHypoIds proof) in
  (* TODO: ajouter les listes des hypothèses splittables, filtrées avec une fonction dans strategies pour avoir la root d'une hypo *)
  let autrelist = if (getRootOfProp (getFirstRemainder proof) = "Implies") then [((fun x y -> intro x), "intro")] else [] in
  (*let autrelist = if (getRootOfProp (getFirstRemainder proof) = "And") then
    ((fun x y z -> andsplit x y z), "andsplit") :: autrelist else autrelist in*)
  (* TBD: rajouter orsplit, modifier andsplit dans strategies et ajouter andsplithypo (qui ne doit pas retourner cote gauche ou droit) *)
  List.concat [applylist; exactlist; autrelist;orsplithypolist];;

(* TODO: changer la façon dont les splits sont faits:
 utiliser l'idée de victor sur le orsplit avec le andsplit *)
(*let backtrack = fun proof ->
  let rec recback = fun proo nameacc ->
    let funcnamelist = buildfunclist proo in
    let rec explorePossibilities = fun fnlist ->
      let (func, funcname) = List.hd fnlist in
      (* tester fonction *)
      let newnameacc = String.concat ">" [nameacc;funcname] in
      let (result, resproof) = func proo in
      let () = Printf.printf "%s (%s)\n" newnameacc (if remainderLines resproof > 1 then "split" else if result then "ok" else "fail") in
      if result then
        if isRemainderTrue resproof then
          (* fin du backtrack, TH prouvé *)
          let () = Printf.printf "(true)\n" in
          true
        else
          if remainderLines resproof > 1 then
            (* Séparer le pb en plusieurs *)
            let pblist = splitProblem resproof in
            let backlist = List.map (fun proo -> let () = Printf.printf "\n" in recback proo newnameacc) pblist in
            let splitResult = List.fold_left (fun x y -> x && y) true backlist in
            let () = Printf.printf "\n%s (split %s: [%s])\n" newnameacc (if splitResult then "ok" else "failed") (String.concat "; " (List.map (fun x -> if x then "true" else "false") backlist)) in
            splitResult
          else
            (* continuer à tester les fonctions à cet étage *)
            recback resproof newnameacc
      else
        if (List.tl fnlist) != [] then
          explorePossibilities (List.tl fnlist)
        else
          let () = Printf.printf "(false)\n" in
          false in
    if funcnamelist != [] then
      explorePossibilities funcnamelist
    else
      let () = Printf.printf "%s (No strategies available)\n" nameacc in
      false in
  recback proof "";;
*)

(* Nouveau code *)

let backtrack = fun proof prints ->
  let rec recback = fun proo nameacc ->
    (* Déterminer la liste des stratégies potentiellement pertinentes à essayer *)
    (* Tester toutes les méthodes
       > regarder le nom de la stratégie actuelle
       > si cette stratégie qui peut split
         > backtracker le côté gauche
         > backtracker le côté droit
         > le theoreme est prouvé si les deux côtés sont prouvés
         > si le theoreme est prouvé
           > arrêter backtrack
         > sinon
           > passer à la stratégie suivante
       > sinon
         > exécuter la stratégie
         > si le theoreme est prouvé
           > arrêter le backtrack
         > sinon
           > si cette stratégie a changé le probleme
             > backtracker sur le nouveau probleme restant
           > sinon
             > arrêter et passer à la prochaine stratégie
       (TBD: ecrire le reste de l'algo)
     *)
    let funclist = buildfunclist proo in
    let rec explore = fun funlist ->
      match funlist with
        hdfunc::rest ->
          begin
            let (func, funcname) = hdfunc in
            (* créer le nouveau accumulateur de nom *)
            let newnameacc = String.concat ">" [nameacc; funcname] in
            if (funcname = "andsplit") then
              let (res_left, resproof_left) = func proo true in
              if (not res_left) then
                (* la commande a échoué d'un des deux côtés, donc des deux*)
                explore rest
              else
                if (recback resproof_left (String.concat "\n" [newnameacc; ""])) then
                  (* le côté gauche a été réussi, essayer le droit *)
                  let (res_right, resproof_right) = func proo false in
                  if (recback resproof_right (String.concat "\n" [newnameacc; ""])) then
                    (* le côté gauche ET le côté droit on prouvé, donc on a prouvé le TH *)
                    let () = Printf.printf "Success\n" in
                    true
                  else
                    explore rest
                else
                  explore rest
            else if (funcname == "orsplit") then
              let (res_left, resproof_left) = func proo true in
              let (res_right, resproof_right) = func proo false in
              if (res_left || res_right) then
                if (recback resproof_left (String.concat "\n" [newnameacc; ""]) || recback resproof_left (String.concat "\n" [newnameacc; ""])) then
                  true
                else
                  explore rest
              else
                explore rest
            else
              (* calculer le résultat de la stratégie *)
              let (result, resproof) = func proo false in
              (* print conditionnel (de debug) *)
              let () = if prints then (Printf.printf "%s (%s)\n" newnameacc (if result then "ok" else "fail")) else () in
              if result then
                if isRemainderTrue resproof then
                  (* Si résolution du ss-pb réussie: print *)
                  let () = Printf.printf "%s (%s)\n" newnameacc (if result then "ok" else "fail") in
                  true
                else
                  recback resproof newnameacc
              else
                explore rest
          end
      | _ -> false in
    explore funclist in
  recback proof "";;
