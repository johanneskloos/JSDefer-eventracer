open ClassifyTask
open Trace

module PostWaitEdge = struct
  type t = POST | HB [@@deriving ord]
  let hash = function POST -> 0 | HB -> 1
  let default = HB
end

module PostWaitGraph =
  Graph.Persistent.Digraph.ConcreteLabeled(DependencyGraph.V)(PostWaitEdge)
module Oper = Graph.Oper.P(PostWaitGraph)

let add_successors lbl r v good g =
  let rec search (g: PostWaitGraph.t) = function
    | v'::tasks ->
        if BatList.mem v' good then
          search (PostWaitGraph.add_edge_e g (v, lbl, v')) tasks
        else
          search g (DependencyGraph.succ r v' @ tasks)
    | [] -> g
  in search g (DependencyGraph.succ r v)

let add_edges_for lbl r good g =
  BatList.fold_left (fun g v -> add_successors lbl r v good g)
    g good

let add_edges hb po good g =
  g
    (* First, walk the post relation. *)
    |> add_edges_for PostWaitEdge.POST po good 
    (* Then, walk the hb relation. *)
    |> add_edges_for PostWaitEdge.HB hb good

let build_post_wait_graph trace classifier =
  let { events; deps } = trace
  in let js_tasks =
    Format.eprintf "js_tasks@.";
    BatList.filter_map
      (fun { id } ->
         try if is_script (IntMap.find id classifier) then Some id else None
         with Not_found -> None)
      events
  in let post_graph =
    Format.eprintf "post_graph@.";
    BatList.fold_left
      (fun g { id=src; commands } ->
         BatList.fold_left
           (fun g -> function
              | Post dst -> DependencyGraph.add_edge g src dst
              | _ -> g)
           (DependencyGraph.add_vertex g src) commands)
      DependencyGraph.empty events
  in let graph =
    Format.eprintf "graph@.";
    BatList.fold_left PostWaitGraph.add_vertex PostWaitGraph.empty js_tasks
    |> add_edges deps post_graph js_tasks
  in Format.eprintf "reduce@.";
     Oper.transitive_reduction graph

