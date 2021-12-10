(* Backtrack.ml *)

open Strategies;;

let buildfunclist = fun proof ->
  let funclist = fun func funcname hypolist ->
    List.map (fun id -> (func id, String.concat " " [funcname;(string_of_int id)])) hypolist in
  let applylist = funclist apply "apply" (getAllHypoIds proof) in
  let exactlist = funclist exact "exact" (getAllHypoIds proof) in
  let autrelist = if (getRootOfProp (getFirstRemainder proof) = "Implies") then [(intro, "intro")] else [] in
  let autrelist = if (getRootOfProp (getFirstRemainder proof) = "And") then
    (andsplit, "andsplit") :: autrelist else autrelist in
  List.concat [applylist; exactlist; autrelist];;

let backtrack = fun proof ->
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
