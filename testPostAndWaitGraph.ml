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

let build_post_wait_graph trace =
  let ({ events; deps }, classifier) = classify trace
  in let js_tasks =
    Format.eprintf "js_tasks@.";
    BatList.filter_map
      (fun { id } ->
         match IntMap.find id classifier with
           | ToplevelScript
           | EventHandlerScript
           | DelayedScript
           | SomeScript -> Some id
           | _ -> None
           | exception Not_found -> None)
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
     (Oper.transitive_reduction graph, classifier)

let format_post_wait_graph chan (graph, classifier) =
  let module DOTFormat = struct
    include PostWaitGraph
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let default_edge_attributes _ = []
    let edge_attributes (_, label, _) = match label with
      | PostWaitEdge.POST -> [ `Style `Solid ]
      | PostWaitEdge.HB -> [ `Style `Dashed ]
    let vertex_attributes v =
      begin match IntMap.find v classifier with
      | ToplevelScript -> `Fillcolor 0xffff00
      | DelayedScript -> `Fillcolor 0x80ff80
      | EventHandlerScript -> `Fillcolor 0x8080ff
      | SomeScript -> `Fillcolor 0xff0000
      | _ -> `Fillcolor 0xff0000
      | exception Not_found -> `Fillcolor 0xff0000
      end :: [ `Style `Filled ]
    let get_subgraph _ = None
    let vertex_name = string_of_int
  end in
  let module DOT = Graph.Graphviz.Dot(DOTFormat)
  in DOT.output_graph chan graph

module BFS = Graph.Traverse.Bfs(PostWaitGraph)
let remove_branches cut_here graph =
  BatList.fold_left
    (fun g v ->
       Format.eprintf "Cutting successors of %d@." v;
       BFS.fold_component
         (fun v' g -> PostWaitGraph.remove_vertex g v')
         g graph v)
    graph cut_here


let handle_log cut_here filename =
  let base = Filename.chop_suffix (Filename.basename filename) ".log" in
  let data =
    filename
      |> CleanLog.load
      |> Trace.parse_trace
      |> build_post_wait_graph
      |> BatTuple.Tuple2.map1 (remove_branches !cut_here)
  in let chan = open_out (base ^ ".dot")
  in try
    format_post_wait_graph chan data;
    flush chan;
    close_out chan
  with e ->
    close_out chan;
    Format.eprintf "Caught excepion while handling %s: %s"
      filename (Printexc.to_string e)

let () =
  let cut_here = ref [] in
    Arg.parse ["-c",
               Arg.Int (fun i -> Format.eprintf "Will cut %d@." i;
                                 cut_here := i :: !cut_here),
               "Cut the branch starting here"]
      (handle_log cut_here) ""
