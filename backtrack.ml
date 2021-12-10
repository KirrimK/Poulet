(* Backtrack.ml *)

open Strategies;;

(* Nouveau Code *)

let buildfunclist = fun proof ->
  let funclist = fun func funcname hypolist ->
    List.map (fun id -> (func id, String.concat " " [funcname;(string_of_int id)])) hypolist in
  let applylist = funclist apply "apply" (getAllHypoIds proof) in
  let exactlist = funclist exact "exact" (getAllHypoIds proof) in
  let autrelist = if (getRootOfProp (getFirstRemainder proof) = "Implies") then [(intro, "intro")] else [] in
  List.concat [applylist; exactlist; autrelist];;

let backtrack = fun proof ->
  let rec recback = fun proo nameacc ->
    let funcnamelist = buildfunclist proo in
    let rec explorePossibilities = fun fnlist ->
      let (func, funcname) = List.hd fnlist in
      (* tester fonction *)
      let newnameacc = String.concat ">" [nameacc;funcname] in
      let () = Printf.printf "%s\n" newnameacc in
      let (result, resproof) = func proo in
      if result then
        if isRemainderTrue resproof then
          (* fin du backtrack, TH prouvé *)
          true
        else
          (* continuer à tester les fonctions à cet étage *)
          recback resproof newnameacc
      else
        if (List.tl fnlist) != [] then
          explorePossibilities (List.tl fnlist)
        else
          false in
    explorePossibilities funcnamelist in
  recback proof "";;
