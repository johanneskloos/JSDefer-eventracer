open Trace
open ReadsWrites
open PostAndWaitGraph
module UnorderedIntPair = struct
  type t = int * int
  let compare (x1, y1) (x2, y2) =
    match compare (min x1 y1) (min x2 y2) with
      | 0 -> compare (max x1 y1) (max x2 y2)
      | c -> c
end
module IntPairSet = BatSet.Make(UnorderedIntPair)

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
let log_red src tgt=
  match !log_channel with
    | Some (_, pp) ->
        Format.fprintf pp "Merge %d %d@," src tgt
    | None -> ()
let log_succs what src tgts =
  match !log_channel with
    | Some (_, pp) ->
        Format.fprintf pp "@[<hov 2>Reduction for %s: %d gets %a@]@,"
          what src (Fmt.list ~sep:Fmt.sp Fmt.int) tgts
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
        Logs.warn ~src:!Log.source
          (fun m -> m "Warning: Reading specific value %a from undetermined read"
                      pp_value v1)
    | Some v1, Some v2 ->
        if v1 <> v2 then
          Logs.warn ~src:!Log.source
            (fun m -> m "Warning: Nondeterministic read, got both %a and %a"
                        pp_value v1 pp_value v2)

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

let task_step ecur { reads; writes } { dependent_reads; last_writes; graph } =
  let graph =
    graph
      |> ReferenceMap.fold (add_rf_edge last_writes ecur) reads
      |> ReferenceMap.fold (add_mo_edges dependent_reads ecur) writes
  and dependent_reads =
    ReferenceMap.merge (merge_dependent_reads ecur) dependent_reads reads
  and last_writes =
    ReferenceMap.merge (merge_last_writes ecur) last_writes writes
  in { graph; dependent_reads; last_writes }

type data = {
  has_dom_write: IntSet.t;
  has_nondeterminism: StringSet.t IntMap.t;
  spec: ReadsWrites.event_standalone_spec IntMap.t;
  po: PostAndWaitGraph.PostWaitGraph.t;
  potential_races: (int * int) list;
  script_short_timeouts: int list;
}

module MergePO = Graph.Merge.P(PostAndWaitGraph.PostWaitGraph)
let merge_successor pre ({ has_nondeterminism; has_dom_write; spec; po } as data) succ =
  log_red pre succ;
  let merge_intset set =
    if IntSet.mem succ set then IntSet.add pre (IntSet.remove succ set) else set
  and merge_nondet map =
    match IntMap.Exceptionless.find succ map with
      | Some nondet ->
          IntMap.modify_def StringSet.empty pre (StringSet.union nondet) map
      | None -> map
  in { data with
    has_dom_write = merge_intset has_dom_write;
    has_nondeterminism = merge_nondet has_nondeterminism;
    spec = IntMap.modify pre
             (fun pre ->
                ReadsWrites.combine_reads_writes pre (IntMap.find succ spec))
             spec
      |> IntMap.remove succ;
    po = MergePO.merge_vertex po [pre; succ]
  }

let gather_post_successors_set pred po todo =
  let rec loop succ = function
    | [] -> succ
    | v::vs ->
        if IntSet.mem v succ then
          loop succ vs
        else
          loop (IntSet.add v succ)
            (PostWaitGraph.fold_succ_e
               (fun (_, lbl, v') vs ->
                  if lbl = PostWaitEdge.POST && pred v' then v' :: vs else vs)
               po v vs)
  in loop IntSet.empty todo

let gather_post_successors cl po v =
  let pred v = match IntMap.find v cl with
    | ClassifyTask.ShortTimerEventHandlerScript
    | ClassifyTask.ImmediateEventHandlerScript -> true
    | _ -> false
  in
  List.tl (IntSet.to_list (gather_post_successors_set pred po [v]))

let xor b1 b2 = if b1 then not b2 else b2
let find_potential_races races (*scripts*) script_set =
  IntPairSet.filter (fun (ev1, ev2) ->
                       xor (List.mem ev1 script_set) (List.mem ev2 script_set))
    races |> IntPairSet.to_list

let merge_successors_for races scripts v cl data =
  (* Gather post successors of v *)
  let open ClassifyTask in
  let post_successors = gather_post_successors cl data.po v in
  log_succs "script" v post_successors;
  let (short_timeouts, immediates) =
    BatList.partition
      (fun v -> IntMap.find v cl = ShortTimerEventHandlerScript)
      post_successors
  in let potential_races =
    find_potential_races races (*scripts*) (v :: short_timeouts @ immediates) @
    data.potential_races
  and script_short_timeouts = short_timeouts @ data.script_short_timeouts
  in log_succs "script (immediate)" v immediates;
    BatList.fold_left (merge_successor v)
       { data with potential_races; script_short_timeouts } immediates

let merge_successors_scripts races scripts cl data =
  IntSet.fold
    (fun v data ->
       if ClassifyTask.is_toplevel_script (IntMap.find v cl) then
         merge_successors_for races scripts v cl data
       else
         data)
    scripts data

let find_dcl_and_onload cl scripts =
  let open ClassifyTask in
  IntSet.filter (fun v -> match IntMap.find v cl with
                   | WindowInteractiveScript
                   | WindowCompleteScript -> true
                   | _ -> false)
    scripts
    |> IntSet.to_list

let merge_post_dcl scripts cl data =
  let open ClassifyTask in
  let dcl_and_onload = find_dcl_and_onload cl scripts
  and pred v = true
  in let succs = IntSet.to_list(gather_post_successors_set pred data.po dcl_and_onload) 
  in match succs  with
    | v::vs ->
        log_succs "DCL" v vs;
        let { spec } = BatList.fold_left (merge_successor v) data vs in
          (IntMap.find v spec).ReadsWrites.reads
    | [] -> ReferenceMap.empty

let filter_graph p g =
  PostWaitGraph.empty
    |> PostWaitGraph.fold_vertex (fun v g -> if p v then
                                    PostWaitGraph.add_vertex g v
                                  else g) g
    |> PostWaitGraph.fold_edges_e (fun ((vs, _, vt) as e) g ->
                                     if p vs && p vt then
                                       PostWaitGraph.add_edge_e g e
                                     else g) g
let filter_irrelevant scripts
      { has_dom_write; has_nondeterminism; spec; po;
        potential_races; script_short_timeouts } =
  { has_dom_write = IntSet.inter has_dom_write scripts;
    has_nondeterminism = IntMap.filter (fun v _ -> IntSet.mem v scripts) has_nondeterminism;
    spec = IntMap.filter (fun v _ -> IntSet.mem v scripts) spec;
    po = filter_graph (fun v -> IntSet.mem v scripts) po;
    potential_races;
    script_short_timeouts }

let reduce races scripts cl data =
  Logs.debug ~src:!Log.source (fun m -> m "Reducing scripts");
  let data' = merge_successors_scripts races scripts cl data
  in (merge_post_dcl scripts cl data',
      filter_irrelevant
        (IntSet.filter
           (fun v -> ClassifyTask.is_toplevel_script
                       (IntMap.find v cl))
           scripts) data')

let calculate_dependency_graph { spec } =
  Logs.debug ~src:!Log.source (fun m -> m "Calculating dependency graph");
  let { graph } =
    IntMap.fold task_step spec
      { graph = DependencyGraph.empty;
        dependent_reads = ReferenceMap.empty;
        last_writes = ReferenceMap.empty }
  in graph

let find_scripts cl =
  Logs.debug ~src:!Log.source (fun m -> m "Finding scripts");
  IntMap.fold (fun v vc scripts ->
                 if ClassifyTask.is_script vc then
                   IntSet.add v scripts
                 else scripts)
    cl IntSet.empty

let reduce_races races =
  List.fold_left (fun races { ev1; ev2 } -> IntPairSet.add (ev1, ev2) races)
    IntPairSet.empty races

let calculate trace =
  let (trace, cl) = ClassifyTask.classify trace in
  let { MarkEvents.has_dom_write; has_nondeterminism } =
    MarkEvents.calculate_markings trace
  and spec = ReadsWrites.per_event_specification trace
  and po = PostAndWaitGraph.build_post_wait_graph trace cl
  and scripts = find_scripts cl
  and reduced_races = reduce_races trace.races
  in let data = { has_dom_write; has_nondeterminism;
                  spec; po; potential_races = [];
                  script_short_timeouts = [] }
  in let (dcl_pre, data') = reduce reduced_races scripts cl data
  in let depgraph = calculate_dependency_graph data'
  in (trace, cl, data, data', dcl_pre, depgraph)

