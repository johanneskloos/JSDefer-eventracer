open ClassifyTask
open Trace

let format_dependency_graph chan graph classifier =
  let module DOTFormat = struct
    include DependencyGraph
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let default_edge_attributes _ = []
    let edge_attributes (_, label, _) = match label with
      | Some d -> [ `Style `Dashed; `Label (string_of_int d) ]
      | None -> [ `Style `Solid ]
    let vertex_attributes =
      ClassificationLayout.vertex_attribute classifier (fun _ -> [])
    let get_subgraph _ = None
    let vertex_name = string_of_int
  end in
  let module DOT = Graph.Graphviz.Dot(DOTFormat)
  in DOT.output_graph chan graph

module RB = RemoveBelow.P(DependencyGraph)

let handle_log reasons remove filename =
  let base = Filename.chop_suffix (Filename.basename filename) ".log" in
    if !reasons then
      OrderGraph.log (open_out (base ^ ".reasons"));
  let (trace, dep, (pw, classification)) =
    filename
      |> CleanLog.load
      |> Trace.parse_trace
      |> OrderGraph.build_graphs
  in let dep' = List.fold_left RB.remove_below dep !remove
  in let _ = remove := []
  in let chan = open_out (base ^ ".dep.dot")
  in try
    format_dependency_graph chan dep' classification;
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
    ("-p", Arg.Int (fun v -> remove := v :: !remove), "remove below this node")
  ] (handle_log reasons remove) ""
