open Trace
module Merge = Graph.Merge.P(DependencyGraph)

let irrelevant commands =
  let rec ends_in_posts_and_cached = function
    | Read (RCachedResource _, _) :: rest
    | Write (RCachedResource _, _) :: rest
    | Post _ :: rest -> ends_in_posts_and_cached rest
    | [Exit] -> true
    | _ -> false
  in match commands with
    | [] -> true
    | Enter (Parse _) :: Write _ :: rest -> ends_in_posts_and_cached rest
    | Enter (Parse _) :: rest -> ends_in_posts_and_cached rest
    | _ -> false

let merge { events; deps } =
  let (empty, nonempty) =
    BatList.partition (fun { commands } -> irrelevant commands)
      events
  in let empty_events = BatList.map (fun { id } -> id) empty
  in let reduced_deps = DependencyGraph.fold_vertex
       (fun src g ->
          let to_remove=
            DependencyGraph.fold_succ
              (fun tgt to_remove ->
                 if BatList.mem tgt empty_events then begin
                   Format.printf "Marking %d for merge with %d@." tgt src;
                   tgt :: to_remove
                 end else
                   to_remove)
              deps src []
          in Merge.merge_vertex g (src::to_remove))
       deps deps
  in 
    { events = nonempty; deps = reduced_deps }

let with_output_file name f =
  let chan = open_out name in
    try f chan; close_out chan
    with e -> close_out chan; raise e

module Dot = Graph.Graphviz.Dot(
struct
  include DependencyGraph
  let edge_attributes (_, duration, _) =
    match duration with
      | Some d -> [ `Label (string_of_int d) ]
      | None -> []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
  let get_subgraph _ = None
  let vertex_name = string_of_int
end)

let reduce_trace filename =
  let base = Filename.chop_suffix (Filename.basename filename) "log"
  and trace = filename
    |> CleanLog.load
    |> Trace.parse_trace
    |> merge
  in with_output_file (base ^ ".trace")
       (fun chan ->
          let pp = Format.formatter_of_out_channel chan
          in Trace.pp_trace_with_deps pp trace;
             Format.pp_print_flush pp ());
     with_output_file (base ^ ".dot")
       (fun chan -> Dot.output_graph chan trace.deps)


let () =
  Arg.parse [] reduce_trace ""
