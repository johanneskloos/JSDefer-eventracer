open ClassifyTask
open Trace
open ReducedOrderGraph
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

let write_classified_trace { events } cl channel =
  let open Fmt in
  let pp = Format.formatter_of_out_channel channel in
    pf pp "@[<v>%a@,@]@."
      (list ~sep:cut (pp_trace_with_classification cl)) events

let pp_spec pp { ReadsWrites.reads; writes; posts } =
  let open Fmt in
    pf pp "Reads: @[<hov>%a@]@,Writes: @[<hov>%a@]@,Posts: @[<hov>%a@]@,"
      (ReferenceMap.pp_default ~esep:sp ~psep:(const string "↦") (option ~none:(const string "(?)") Trace.pp_value)) reads
      (ReferenceMap.pp_default ~esep:sp ~psep:(const string "↦") (option ~none:(const string "(?)") Trace.pp_value)) writes
      (IntSet.pp ~sep:sp) posts

let write_data { ReducedOrderGraph.has_dom_write; has_nondeterminism;
                 spec; po; potential_races; script_short_timeouts }
      chan =
  let open Fmt in
  let pp = Format.formatter_of_out_channel chan in
    pf pp "@[<v>Events with DOM writes: @[<hov>%a@]@,\
                Events with nondeterminism: @[<hov>%a@]@,\
                Potential races: @[<hov>%a@]@,\
                Short-timeout events caused by scripts: @[<hov>%a@]@,\
                Specifications:@,%a@]@."
      (IntSet.pp ~sep:sp) has_dom_write
      (IntSet.pp ~sep:sp) has_nondeterminism
      (list ~sep:sp (pair ~sep:(const string "-") int int)) potential_races
      (list ~sep:sp int) script_short_timeouts
      (IntMap.pp ~sep:cut (vbox ~indent:2 (pair ~sep:sp int pp_spec))) spec
      
let write_pre dcl_pre chan =
  let open Fmt in
  let pp = Format.formatter_of_out_channel chan in
    pf pp "@[<v>%a@]@."
      (ReferenceMap.pp_default ~psep:(const string ":") (option Trace.pp_value)) dcl_pre


let handle_log remove filename =
  let base = Filename.chop_suffix (Filename.basename filename) ".log" in
  let (trace, cl, data, data', dcl_pre, depgraph) =
    CleanLog.load filename
      |> Trace.parse_trace
      |> ReducedOrderGraph.calculate
  in with_out_file (base ^ ".cltr") (write_classified_trace trace cl);
     with_out_file (base ^ ".data") (write_data data);
     with_out_file (base ^ ".red-data") (write_data data');
     with_out_file (base ^ ".po.dot")
       (ClassificationLayout.output_post_wait_graph data.po cl (fun _ -> []));
     with_out_file (base ^ ".red-po.dot")
       (ClassificationLayout.output_post_wait_graph data'.po cl (fun _ -> []));
     with_out_file (base ^ ".dclpre")
       (write_pre dcl_pre);
     with_out_file (base ^ ".dep.dot") 
       (ClassificationLayout.output_dependency_graph depgraph cl
          (fun _ -> [])
          (fun (_, lbl, _) -> if lbl = None then [ `Style `Solid ] else [ `Style `Dotted ]))


let () =
  let remove = ref [] in
  Arg.parse [
    ("-G", Arg.Set OrderGraph.guid_heuristic, "GUID heuristic (HACK)");
    ("-l", Arg.String (fun fn -> ReducedOrderGraph.log (open_out fn)), "log file")
  ] (handle_log remove) ""
