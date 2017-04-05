open Trace

let log_red src tgt=
  DetailLog.log
    (fun f -> f "Merge %d %d@," src tgt)
let log_succs what src tgts =
  DetailLog.log
    (fun f -> f "@[<hov 2>Reduction for %s: %d gets %a@]@,"
          what src (Fmt.list ~sep:Fmt.sp Fmt.int) tgts)

type trace_facts = {
  trace: Trace.trace;
  classification: ClassifyTask.classification IntMap.t;
  has_dom_write: IntSet.t;
  has_nondeterminism: StringSet.t IntMap.t;
  spec: ReadsWrites.event_standalone_spec IntMap.t;
  po: PostAndWaitGraph.PostWaitGraph.t;
  potential_races: Races.RaceSet.t;
  script_short_timeouts: int list;
  dependency_graph: Trace.DependencyGraph.t
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
  let open PostAndWaitGraph in
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

let merge_successors_for races v cl data =
  (* Gather post successors of v *)
  let open ClassifyTask in
  let post_successors = gather_post_successors cl data.po v in
  log_succs "script" v post_successors;
  let (short_timeouts, immediates) =
    BatList.partition
      (fun v -> IntMap.find v cl = ShortTimerEventHandlerScript)
      post_successors
  in let potential_races =
    Races.RaceSet.union
      (Races.find_potential_races races (v :: short_timeouts @ immediates))
      data.potential_races
  and script_short_timeouts = short_timeouts @ data.script_short_timeouts
  in log_succs "script (immediate)" v immediates;
     Logs.debug ~src:!Log.source
       (fun m -> m "@[<hov 4>potential races: %a]" Races.pp_races potential_races);
    BatList.fold_left (merge_successor v)
       { data with potential_races; script_short_timeouts } immediates

let merge_successors_scripts races scripts cl data =
  IntSet.fold
    (fun v data ->
       if ClassifyTask.is_toplevel_script (IntMap.find v cl) then
         merge_successors_for races v cl data
       else
         data)
    scripts data

    (*
let find_dcl_and_onload cl scripts =
  let open ClassifyTask in
  IntSet.filter (fun v -> match IntMap.find v cl with
                   | WindowInteractiveScript
                   | WindowCompleteScript -> true
                   | _ -> false)
    scripts
    |> IntSet.to_list

     *)
    (*
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
     *)

let filter_graph p g =
  let open PostAndWaitGraph in
  PostWaitGraph.empty
    |> PostWaitGraph.fold_vertex (fun v g -> if p v then
                                    PostWaitGraph.add_vertex g v
                                  else g) g
    |> PostWaitGraph.fold_edges_e (fun ((vs, _, vt) as e) g ->
                                     if p vs && p vt then
                                       PostWaitGraph.add_edge_e g e
                                     else g) g
let filter_irrelevant scripts
      ({ has_dom_write; has_nondeterminism; spec; po } as data) =
  { data with
    has_dom_write = IntSet.inter has_dom_write scripts;
    has_nondeterminism = IntMap.filter (fun v _ -> IntSet.mem v scripts) has_nondeterminism;
    spec = IntMap.filter (fun v _ -> IntSet.mem v scripts) spec;
    po = filter_graph (fun v -> IntSet.mem v scripts) po }

let reduce races scripts cl data =
  Logs.debug ~src:!Log.source (fun m -> m "Reducing scripts");
  let data' = merge_successors_scripts races scripts cl data
  in Logs.debug ~src:!Log.source (fun m -> m "@[<hov 4>Races: %a@]"
                                             Races.pp_races data'.potential_races);
      filter_irrelevant
        (IntSet.filter
           (fun v -> ClassifyTask.is_toplevel_script
                       (IntMap.find v cl))
           scripts) data'

let find_scripts cl =
  Logs.debug ~src:!Log.source (fun m -> m "Finding scripts");
  IntMap.fold (fun v vc scripts ->
                 if ClassifyTask.is_script vc then
                   IntSet.add v scripts
                 else scripts)
    cl IntSet.empty

let calculate trace =
  let (trace, cl) = ClassifyTask.classify trace in
  let { MarkEvents.has_dom_write; has_nondeterminism } =
    MarkEvents.calculate_markings trace
  and spec = ReadsWrites.per_event_specification trace
  and po = PostAndWaitGraph.build_post_wait_graph trace cl
  and scripts = find_scripts cl
  in let data = { trace; classification = cl;
                  has_dom_write; has_nondeterminism;
                  spec; po; potential_races = Races.RaceSet.empty;
                  script_short_timeouts = [];
                  dependency_graph = DependencyGraph.empty }
  in let data' = reduce trace.races scripts cl data
  in let depgraph = CalculateRFandMO.calculate_dependency_graph data'.spec
  in { data with dependency_graph = depgraph }

