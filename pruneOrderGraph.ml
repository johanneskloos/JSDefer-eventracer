open ClassifyTask
open Trace

type pruned_order_graph = {
  trace: trace;
  classification: classification IntMap.t;
  post_wait_graph: PostAndWaitGraph.PostWaitGraph.t;
  basic_order_graph: DependencyGraph.t;
  pruned_order_graph: DependencyGraph.t
}

module RB = RemoveBelow.P(DependencyGraph)
module BFS = Graph.Traverse.Bfs(PostAndWaitGraph.PostWaitGraph)

let prune_successors order_graph post_wait_graph nodes =
  List.fold_left
    (fun g v ->
       if PostAndWaitGraph.PostWaitGraph.mem_vertex post_wait_graph v then
         BFS.fold_component (fun v' g -> RB.remove_below g v')
           g post_wait_graph v
       else g)
    order_graph nodes

let prune_order_graph pruning_strategy trace =
  let (trace, basic_order_graph, post_wait_graph, classification, specs) =
    OrderGraph.build_graphs trace
  in let nodes_to_remove =
    pruning_strategy trace basic_order_graph post_wait_graph classification
  in let pruned_order_graph =
    prune_successors basic_order_graph post_wait_graph nodes_to_remove
  in { trace; classification; post_wait_graph; basic_order_graph; pruned_order_graph }

let pruning_heuristics trace basic_order_graph post_wait_graph classification =
  (* Prune everything that is not a script, or a long-term event handler. *)
  IntMap.fold (fun v vc remove ->
                 match vc with
                   | ImmediateEventHandlerScript
                   | InlineScript
                   | ExternalSyncScript
                   | ExternalAsyncScript
                   | ExternalDeferScript
                   | ExternalUnknownScript -> remove
                   | _ -> v :: remove)
    classification []
