open Trace

let guid_heuristic = ref false

let log_add_edge reason ref efrom eto =
  DetailLog.log
    (fun f -> f "%s %a: %d -> %d@," reason pp_reference ref efrom eto)

type state = {
  dependent_reads: (int list * value option) ReferenceMap.t;
  last_writes: (value option * int) ReferenceMap.t;
  graph: DependencyGraph.t
}


let warn_read_incompatibility v1 v2 =
  match v1, v2 with
    | None, _ -> ()
    | Some v1, None ->
        Log.warn
          (fun m -> m "Warning: Reading specific value %a from undetermined read"
                      pp_value v1)
    | Some v1, Some v2 ->
        if v1 <> v2 then
          Log.warn
            (fun m -> m "Warning: Nondeterministic read, got both %a and %a"
                        pp_value v1 pp_value v2)


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

let task_step ecur { ReadsWrites.reads; writes }
      { dependent_reads; last_writes; graph } =
  let open ReadsWrites in
  let graph =
    graph
      |> ReferenceMap.fold (add_rf_edge last_writes ecur) reads
      |> ReferenceMap.fold (add_mo_edges dependent_reads ecur) writes
  and dependent_reads =
    ReferenceMap.merge (merge_dependent_reads ecur) dependent_reads reads
  and last_writes =
    ReferenceMap.merge (merge_last_writes ecur) last_writes writes
  in { graph; dependent_reads; last_writes }

let calculate_dependency_graph spec =
  Log.debug (fun m -> m "Calculating dependency graph");
  let { graph } =
    IntMap.fold task_step spec
      { graph = DependencyGraph.empty;
        dependent_reads = ReferenceMap.empty;
        last_writes = ReferenceMap.empty }
  in graph

