(* Backtrack.ml *)

open Hypothese;;
open Strategies;;

type node = {
    state: proof;
    cmdhasok: bool;
    cmdname: string;
    children: node list;
};;

let createstratlist = fun proof ->
  let funclist = fun func funcname hypolist ->
    List.map (fun hypo -> (func hypo.id, String.concat " " [funcname;(string_of_int hypo.id)])) hypolist in
  let applylist = funclist apply "apply" proof.hypos in
  let exactlist = funclist exact "exact" proof.hypos in
  let autrelist = [(intro, "intro")] in
  List.concat [applylist; exactlist; autrelist];;

let backtrack = fun proo ->
  let makenode = fun proof funcandfuncname ->
    let (func, funcname) = funcandfuncname in
    let (newok, newst) = func proof in
    {state=newst; cmdhasok=newok; cmdname=funcname; children=[]} in
  let rec expandnode = fun node ->
    if not node.cmdhasok then
      node
    else
      let state = node.state in
      let stratlist = createstratlist state in
      let childlist = List.map (makenode state) stratlist in
      let expandedchildlist = List.map expandnode childlist in
      {state=state; cmdhasok=node.cmdhasok; cmdname=node.cmdname; children=expandedchildlist} in
  expandnode (makenode proo ((fun x->(true, proo)),""));;

