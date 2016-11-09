open ClassifyTask
open Trace
open PruneOrderGraph
open ClassificationLayout

let with_out_file filename f =
  let chan = open_out filename in try
    f chan;
    close_out chan
  with e ->
    close_out chan;
    raise e

let pp_trace_with_classification cl pp { id; evtype; commands } =
  let open Fmt in
    pf pp "@[<v2>Event %d: %a, %a {@,%a@]@,}"
      id
      pp_event_action_type evtype
      pp_classification (IntMap.find id cl)
      (list ~sep:cut pp_command) commands

let write_classified_trace { events } cl late_precondition channel =
  let open Fmt in
  let pp = Format.formatter_of_out_channel channel in
    pf pp "@[<v>Late precondition: %a@,@,%a@,@]@."
      (fun pp _ -> string pp "???") late_precondition
      (list ~sep:cut (pp_trace_with_classification cl)) events

let handle_log remove filename =
  let base = Filename.chop_suffix (Filename.basename filename) ".log" in
  let (trace, cl, po1, po2, po3, dep1, dep2, dep3, late_precondition) =
    CleanLog.load filename
      |> Trace.parse_trace
      |> ReduceOrderGraph.build_reduced_order_graph
  in with_out_file (base ^ ".cltr") (write_classified_trace trace cl late_precondition);
     with_out_file (base ^ ".po.dot")
       (output_post_wait_graph po1 cl (fun _ -> []));
     with_out_file (base ^ ".po-imm.dot")
       (output_post_wait_graph po2 cl (fun _ -> []));
     with_out_file (base ^ ".po-pruned.dot")
       (output_post_wait_graph po3 cl (fun _ -> []));
     with_out_file (base ^ ".dep.dot")
       (output_dependency_graph dep1 cl (fun _ -> []) (fun _ -> []));
     with_out_file (base ^ ".dep-imm.dot")
       (output_dependency_graph dep2 cl (fun _ -> []) (fun _ -> []));
     with_out_file (base ^ ".dep-pruned.dot")
       (output_dependency_graph dep3 cl (fun _ -> []) (fun _ -> []))

let () =
  let remove = ref [] in
  Arg.parse [
    ("-G", Arg.Set OrderGraph.guid_heuristic, "GUID heuristic (HACK)");
  ] (handle_log remove) ""
