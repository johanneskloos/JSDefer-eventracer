module DependencyGraph = Trace.DependencyGraph

module PostWaitEdge = struct
  type t = POST | HB [@@deriving ord]
  let hash = function POST -> 0 | HB -> 1
  let default = HB
end

module PostWaitGraph =
  Graph.Persistent.Digraph.ConcreteLabeled(DependencyGraph.V)(PostWaitEdge)
module Oper = Graph.Oper.P(PostWaitGraph)

let get_successors g v =
  if DependencyGraph.mem_vertex g v then
    DependencyGraph.succ g v
  else [] (* This is a rare case, but it may happen.
           * Compare the kindredhealthcare trace. *)

let add_successors lbl r v good g =
  Log.debug (fun m -> m "Sucessors for %d" v);
  let seen = Hashtbl.create 17 in
  let rec search (g: PostWaitGraph.t) = function
    | v'::tasks ->
        if Hashtbl.mem seen v' then begin
          Log.debug
            (fun m -> m "Skipping %d, it has been seen before" v');
          g
        end else begin
          Hashtbl.add seen v' ();
          if Hashtbl.mem good v' then begin
            Log.debug
              (fun m -> m "Reached %d, #remaining tasks: %d"
                          v' (List.length tasks));
            search (PostWaitGraph.add_edge_e g (v, lbl, v')) tasks
          end else
            search g (get_successors r v' @ tasks)
        end
    | [] -> g
  in search g (get_successors r v)

let add_edges_for lbl r good good_set g =
  Log.debug (fun m -> m "Iterating edges");
  BatList.fold_left (fun g v -> add_successors lbl r v good_set g)
    g good

let add_edges hb po good g =
  let good_set = Hashtbl.create (BatList.length good) in
    List.iter (fun v -> Hashtbl.add good_set v 0) good;
    Log.debug (fun m -> m "Adding edges");
    g
    (* First, walk the post relation. *)
    |> add_edges_for PostWaitEdge.POST po good good_set
       (* Then, walk the hb relation. *)
       |> add_edges_for PostWaitEdge.HB hb good good_set

let build_post_wait_graph trace classifier =
  let open Trace in
  Log.debug (fun m -> m "Building post/wait graph");
  let { events; deps } = trace
  in let js_tasks =
    BatList.filter_map
      (fun { id } ->
         try if ClassifyTask.is_script (IntMap.find id classifier)
         then Some id else None
         with Not_found -> None)
      events
  in let post_graph =
    Log.debug (fun m -> m "Building post graph");
    BatList.fold_left
      (fun g { id=src; commands } ->
         BatList.fold_left
           (fun g -> function
              | Post dst -> DependencyGraph.add_edge g src dst
              | _ -> g)
           (DependencyGraph.add_vertex g src) commands)
      DependencyGraph.empty events
  in let graph =
    Log.debug (fun m -> m "Making initial post/wait graph");
    BatList.fold_left PostWaitGraph.add_vertex PostWaitGraph.empty js_tasks
    |> add_edges deps post_graph js_tasks
  in 
    Log.debug (fun m -> m "Reduce post/wait graph");
    Oper.transitive_reduction graph

