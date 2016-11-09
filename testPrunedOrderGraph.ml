open ClassifyTask
open Trace
open PruneOrderGraph

let format_dependency_graph chan graph classifier dom_accesses =
  let module DOTFormat = struct
    include DependencyGraph
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let default_edge_attributes _ = []
    let edge_attributes (_, label, _) = match label with
      | Some d -> [ `Style `Dashed; `Label (string_of_int d) ]
      | None -> [ `Style `Solid ]
    let vertex_attributes =
      ClassificationLayout.vertex_attribute classifier
        (fun v -> if IntSet.mem v dom_accesses then
           [`Shape `Box ]
         else [])
    let get_subgraph _ = None
    let vertex_name = string_of_int
  end in
  let module DOT = Graph.Graphviz.Dot(DOTFormat)
  in DOT.output_graph chan graph

let dom_reference = function
  | RDOMNode _
  | RTree _
  | RDOMNodeAttribute _ -> true
  | _ -> false

let dom_access cmds =
  let rec check scope cmds = match scope, cmds with
    | None, Enter s :: cmds when is_javascript_scope s ->
        check (Some 0) cmds
    | Some i, Enter _ :: cmds ->
        check (Some (i+1)) cmds
    | Some 0, Exit :: cmds ->
        check None cmds
    | Some i, Exit ::  cmds ->
        check (Some (i-1)) cmds
    | Some _, Write (ref, _) :: cmds ->
        dom_reference ref || check scope cmds
    | _, _::cmds ->
        check scope cmds
    | _, [] -> false
  in check None cmds

let handle_log reasons remove filename =
  let base = Filename.chop_suffix (Filename.basename filename) ".log" in
    if !reasons then
      OrderGraph.log (open_out (base ^ ".reasons"));
  let { trace; pruned_order_graph; classification } =
    filename
      |> CleanLog.load
      |> Trace.parse_trace
      |> PruneOrderGraph.prune_order_graph (fun _ _ _ _ -> !remove)
  in let _ = remove := []
  and chan = open_out (base ^ ".dep.dot")
  and dom_accesses =
    BatList.fold_left
      (fun dom { id; commands } ->
         if dom_access commands then IntSet.add id dom else dom)
      IntSet.empty trace.events
  in try
    format_dependency_graph chan pruned_order_graph classification dom_accesses;
    flush chan;
    close_out chan
  with e ->
    close_out chan;
    Format.eprintf "Caught excepion while handling %s: %s"
      filename (Printexc.to_string e)

let () =
  let reasons = ref false
  and remove = ref [] in
  Arg.parse [
    ("-r", Arg.Set reasons, "record reasons");
    ("-p", Arg.Int (fun v -> remove := v :: !remove), "remove below this node");
    ("-G", Arg.Set OrderGraph.guid_heuristic, "GUID heuristic (HACK)");
  ] (handle_log reasons remove) ""
