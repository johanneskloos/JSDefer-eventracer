open Trace
open ReadsWrites
open PostAndWaitGraph

let log_channel = ref None
let log channel =
  let pp = Format.formatter_of_out_channel channel in
    Format.pp_open_vbox pp 0;
    log_channel := Some (channel, pp)
let log_add_edge reason ref efrom eto =
  match !log_channel with
    | Some (_, pp) ->
        Format.fprintf pp "%s %a: %d -> %d@," reason pp_reference ref efrom eto
    | None -> ()
let close_log () = match !log_channel with
  | Some (chan, pp) ->
      Format.pp_close_box pp ();
      Format.pp_print_flush pp ();
      close_out chan
  | None -> ()

type state = {
  dependent_reads: (int list * value option) ReferenceMap.t;
  last_writes: (value option * int) ReferenceMap.t;
  graph: DependencyGraph.t
}

let warn_read_incompatibility v1 v2 =
  match v1, v2 with
    | None, _ -> ()
    | Some v1, None ->
        Format.eprintf
          "Warning: Reading specific value %a from undetermined read@."
          pp_value v1
    | Some v1, Some v2 ->
        if v1 <> v2 then
          Format.eprintf
            "Error: Nondeterministic read, got both %a and %a@."
            pp_value v1 pp_value v2

let guid_heuristic = ref false

let add_rf_edge last_writes ecur ref v1 graph =
  match ref with
    | RHeap { prop = "guid" } when !guid_heuristic -> graph
    | _ -> try
        let (v2, ewrite) = ReferenceMap.find ref last_writes
        in warn_read_incompatibility v1 v2;
           log_add_edge "rf" ref ewrite ecur;
           DependencyGraph.add_edge graph ewrite ecur
      with Not_found -> graph


let add_mo_edges dependent_reads ecur ref v1 graph =
  match ref with
    | RHeap { prop = "guid" } when !guid_heuristic -> graph
    | _ -> try
        let (deps, v2) = ReferenceMap.find ref dependent_reads in
          match v1, v2 with
            | Some v1, Some v2 when v1 = v2 ->
                (* Dummy write, don't care *)
                graph
            | _ ->
                BatList.fold_left (fun graph edep ->
                                     log_add_edge "mo" ref edep ecur;
                                     DependencyGraph.add_edge graph edep ecur)
                  graph deps
      with Not_found -> graph

let merge_dependent_reads ecur (_: reference) dep_reads read =
  match dep_reads, read with
    | Some (deps, Some v1), Some (Some v2) ->
        if v1 <> v2 then
          Format.eprintf "Warning: Nondeterministic reads, saw both %a and %a@."
            pp_value v1 pp_value v2;
        Some (ecur :: deps, Some v2)
    | Some (deps, v), Some None ->
        Some (ecur :: deps, v)
    | Some (deps, None), Some v ->
        Some (ecur :: deps, v)
    | Some _, None ->
        dep_reads
    | None, Some v ->
        Some ([ecur], v)
    | None, None -> None
let merge_last_writes ecur (_: reference) last_write write =
  match write with
    | Some w -> Some (w, ecur)
    | None -> last_write

let dependency_graph trace =
  let reads_writes = per_event_specification trace
  in let { graph } = IntMap.fold
       (fun ecur { reads; writes }
              { dependent_reads; last_writes; graph } ->
          let graph =
            graph
              |> ReferenceMap.fold (add_rf_edge last_writes ecur) reads
              |> ReferenceMap.fold (add_mo_edges dependent_reads ecur) writes
          and dependent_reads =
            ReferenceMap.merge (merge_dependent_reads ecur) dependent_reads reads
          and last_writes =
            ReferenceMap.merge (merge_last_writes ecur) last_writes writes
          in { graph; dependent_reads; last_writes })
       reads_writes
       { graph = DependencyGraph.empty;
         dependent_reads = ReferenceMap.empty;
         last_writes = ReferenceMap.empty }
  in graph

let build_graphs trace =
  let dep = dependency_graph trace
  and pw = build_post_wait_graph trace
  in close_log (); (trace, dep, pw)
