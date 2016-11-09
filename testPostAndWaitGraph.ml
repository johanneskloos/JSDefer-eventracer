open ClassifyTask
open Trace
open PostAndWaitGraph

let format_post_wait_graph chan graph classifier =
  let module DOTFormat = struct
    include PostWaitGraph
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let default_edge_attributes _ = []
    let edge_attributes (_, label, _) = match label with
      | PostWaitEdge.POST -> [ `Style `Solid ]
      | PostWaitEdge.HB -> [ `Style `Dashed ]
    let vertex_attributes =
      ClassificationLayout.vertex_attribute classifier
        (fun _ -> [])
    let get_subgraph _ = None
    let vertex_name = string_of_int
  end in
  let module DOT = Graph.Graphviz.Dot(DOTFormat)
  in DOT.output_graph chan graph

module RB = RemoveBelow.P(PostWaitGraph)
let remove_branches cut_here graph =
  BatList.fold_left
    (fun g v ->
       Format.eprintf "Cutting successors of %d@." v;
         RB.remove_below g v)
    graph cut_here


let handle_log cut_here filename =
  let base = Filename.chop_suffix (Filename.basename filename) ".log" in
  let (trace, classification) =
    filename |> CleanLog.load |> Trace.parse_trace |> ClassifyTask.classify
  in let data =
      build_post_wait_graph trace classification
      |> remove_branches !cut_here
  in let chan = open_out (base ^ ".dot")
  in try
    format_post_wait_graph chan data classification;
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
