open ClassifyTask
open Trace
open ReducedOrderGraph
open ClassificationLayout
open Domination

let with_out_file filename f =
  let chan = open_out filename in try
    f chan;
    close_out chan
  with e ->
    close_out chan;
    raise e

let with_formatter filename f =
  with_out_file filename
    (fun chan ->
       let pp = Format.formatter_of_out_channel chan in
         Format.pp_open_vbox pp 0;
         f pp;
         Format.pp_close_box pp ();
         Format.pp_print_flush pp ())

let write_to_file filename (fmt: 'a Fmt.t) data =
  with_formatter filename (fun pp -> fmt pp data)

let pp_trace_with_classification cl pp { id; evtype; commands } =
  let open Fmt in
    pf pp "@[<v2>Event %d: %a, %a {@,%a@]@,}"
      id
      pp_event_action_type evtype
      pp_classification (try IntMap.find id cl with Not_found -> failwith "missing cl")
      (list ~sep:cut pp_command) commands

let write_classified_trace filename { events } cl =
  write_to_file filename
    Fmt.(list ~sep:cut (pp_trace_with_classification cl)) events

let pp_access =
  let open Fmt in
  ReferenceMap.pp_default ~esep:sp ~psep:(const string "=")
    (option ~none:(const string "(?)") Trace.pp_value)

let pp_spec pp { ReadsWrites.reads; writes; posts } =
  let open Fmt in
    pf pp "Reads: @[<hov>%a@]@,Writes: @[<hov>%a@]@,Posts: @[<hov>%a@]@,"
      pp_access reads
      pp_access writes
      (IntSet.pp ~sep:sp) posts

let pp_data pp { ReducedOrderGraph.has_dom_write; has_nondeterminism;
                 spec; po; potential_races; script_short_timeouts } =
  let open Fmt in
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


let write_data filename = write_to_file filename pp_data
      
let write_pre filename dcl_pre =
  let open Fmt in
    write_to_file filename
      (ReferenceMap.pp_default ~psep:(const string ":") (option Trace.pp_value)) dcl_pre

let analyze log filename =
  let base = Filename.chop_suffix (Filename.basename filename) ".log" in
    if !log then ReducedOrderGraph.log (open_out (base ^ ".details"));
    let (trace, cl, data, data', dcl_pre, depgraph, dom, def) =
      CleanLog.load filename
        |> Trace.parse_trace
        |> Domination.calculate_domination
    in
      write_classified_trace (base ^ ".cltr") trace cl;
      write_data (base ^ ".data") data;
      write_data (base ^ ".red-data") data';
      with_out_file (base ^ ".po.dot")
        (ClassificationLayout.output_post_wait_graph data.po cl (fun _ -> []));
      with_out_file (base ^ ".red-po.dot")
        (ClassificationLayout.output_post_wait_graph data'.po cl (fun _ -> []));
      write_pre (base ^ ".dclpre") dcl_pre;
      with_out_file (base ^ ".dep.dot") 
        (ClassificationLayout.output_dependency_graph depgraph cl
           (fun _ -> [])
           (fun (_, lbl, _) ->
              if lbl = None then
                [ `Style `Solid ]
              else
                [ `Style `Dotted ]));
      write_to_file (base ^ ".dom")
        Fmt.(using (IntMap.filter_map (fun i _ -> try Some (dom i) with Not_found -> None))
               (IntMap.pp_default ~esep:cut ~psep:(const string ": ")
                  (box ~indent:2 pp_analysis_result)))
        cl;
      write_to_file (base ^ ".verdict")
        Fmt.(IntMap.pp_default ~esep:cut ~psep:(const string ": ")
               pp_result)
        def
