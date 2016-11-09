open ReadsWrites
open Trace
open PostAndWaitGraph
open ClassifyTask
open OrderGraph

let (>>) x f = f x; x

let find_scripts cl: int list =
  IntMap.fold (fun v vc vs -> if is_script vc then v::vs else vs)
    cl []
    |> BatList.sort BatInt.compare
    >> Format.eprintf "Scripts: %a@." Fmt.(list ~sep:sp int)

let calculate_immediate_followers po cl v =
  let rec loop vs = function
    | [] -> vs
    | v :: todo ->
        if List.mem v vs then loop vs todo else
          let todo = PostWaitGraph.fold_succ
                        (fun v' todo ->
                           match IntMap.find v' cl with
                             | ImmediateEventHandlerScript ->
                                 v' :: todo
                             | _ -> todo
                             | exception Not_found -> todo)
                        po v todo
          in loop (v::vs) todo
  in loop [] [v]
       >> Format.eprintf "Immediate followers of %d: %a@." v Fmt.(list ~sep:sp int)

(* Implement later *)
let check_race_condition v immediate_followers dep = ()

module MPO = Graph.Merge.P(PostWaitGraph)
module MDG = Graph.Merge.P(DependencyGraph)
let merge_po po v vs = MPO.merge_vertex po (v::vs)
let merge_dep dep v vs = MDG.merge_vertex dep (v::vs)

let merge_immediates scripts dep po cl =
  BatList.fold_left
    (fun (dep, po) v ->
       if PostWaitGraph.mem_vertex po v then begin
         let immediate_followers = calculate_immediate_followers po cl v
         in check_race_condition v immediate_followers dep;
            Format.eprintf "Merging immediate followers of %d@." v;
            let po = merge_po po v immediate_followers
            and dep = merge_dep dep v immediate_followers
            in (dep, po)
       end else (dep, po))
    (dep, po)
    scripts

module POBFS = Graph.Traverse.Bfs(PostWaitGraph)
let find_late po cl =
  let dcl_and_onload =
    IntMap.fold (fun v vc vs -> match vc with
                   | WindowInteractiveScript
                   | WindowCompleteScript ->
                       v::vs
                   | _ -> vs)
      cl []
  in let late =
    BatList.fold_left
      (fun v vs -> POBFS.fold_component IntSet.add v po vs)
      IntSet.empty dcl_and_onload
  in Format.eprintf "@[<v>dcl_and_onload: %a@,late: %a@]@."
       Fmt.(list ~sep:sp int) dcl_and_onload
       (IntSet.pp ~sep:Fmt.sp) late;
    (dcl_and_onload, late)

let calculate_post_dcl_precondition late rw po cl =
  ReferenceMap.empty (* TODO *)

let remove_po_nonscript_and_late late po cl =
  let po = IntSet.fold (fun v po -> PostWaitGraph.remove_vertex po v)
             late po
  in IntMap.fold (fun v vc po ->
                    if is_script vc then po
                    else begin
                      Format.eprintf "%d not a script, removing from po@." v;
                      PostWaitGraph.remove_vertex po v
                    end)
       cl po

let remove_nonscript_and_late late dep cl =
  let dep = IntSet.fold (fun v dep -> DependencyGraph.remove_vertex dep v)
             late dep
  in IntMap.fold (fun v vc dep ->
                    if is_script vc then dep
                    else begin
                      Format.eprintf "%d not a script, removing from dep@." v;
                      DependencyGraph.remove_vertex dep v
                    end)
       cl dep

let build_reduced_order_graph trace =
  let (trace, cl) = classify trace
  in let (dep1, rw) = dependency_graph trace
  and po1 = build_post_wait_graph trace cl
  and scripts = find_scripts cl
  in let (dep2, po2) = merge_immediates scripts dep1 po1 cl
  and (dcl_and_onload, late) = find_late po1 cl
  in let late_precondition =
    calculate_post_dcl_precondition dcl_and_onload rw po2 cl
  in let po3 = remove_po_nonscript_and_late late po2 cl
  in let dep3 = remove_nonscript_and_late late dep2 cl
  in (trace, cl, po1, po2, po3, dep1, dep2, dep3, late_precondition)
