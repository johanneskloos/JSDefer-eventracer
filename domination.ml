open Trace

type analysis_result = {
  dom_accesses: IntSet.t;
  inline_scripts: IntSet.t;
  async_scripts: IntSet.t;
  nondet: StringSet.t
}
let pp_analysis_result pp { dom_accesses; inline_scripts; async_scripts } =
  let open Fmt in
  let print = [("DOM accesses", dom_accesses);
               ("Inline scripts", inline_scripts);
               ("Async scripts", async_scripts)]
  in let print = BatList.filter (fun (_, s) -> not (IntSet.is_empty s)) print
  in if print = [] then
    string pp "(nothing)"
  else 
    list ~sep:(suffix sp (const string ";"))
      (box ~indent:2 (pair ~sep:(const string ": ")
                        string (IntSet.pp ~sep:sp)))
      pp print

module AnalysisStrategy = struct
  type data = analysis_result
  type edge = DependencyGraph.edge
  type vertex = DependencyGraph.vertex
  type g = DependencyGraph.t
  let direction = Graph.Fixpoint.Backward
  let join
        { dom_accesses = d1; nondet = n1;
          inline_scripts = i1; async_scripts = a1 }
        { dom_accesses = d2; nondet = n2;
          inline_scripts = i2; async_scripts = a2 } =
    { dom_accesses = IntSet.union d1 d2;
      inline_scripts = IntSet.union i1 i2;
      async_scripts = IntSet.union a1 a2;
      nondet = StringSet.union n1 n2
    }
  let equal 
        { dom_accesses = d1; nondet = n1;
          inline_scripts = i1; async_scripts = a1 }
        { dom_accesses = d2; nondet = n2;
          inline_scripts = i2; async_scripts = a2 } =
    IntSet.equal d1 d2 &&
      IntSet.equal i1 i2 &&
      IntSet.equal a1 a2 &&
      StringSet.equal n1 n2

  let analyze (_: edge) (r: analysis_result) = r
end

module DominationAnalysis =
  Graph.Fixpoint.Make(DependencyGraph)(AnalysisStrategy)
let calculate_domination has_nondeterminism has_dom_write cl depgraph =
  Logs.debug ~src:!Log.source (fun m -> m "Calculating domination facts");
  try
    let cond_singleton p v =
      if p then IntSet.singleton v else IntSet.empty
    in let maybe_singleton s v =
      cond_singleton (IntSet.mem v s) v
    in DominationAnalysis.analyze
         (fun v ->
            let open ClassifyTask in
              { dom_accesses = maybe_singleton has_dom_write v;
                inline_scripts =
                  cond_singleton (IntMap.find v cl = InlineScript) v;
                async_scripts =
                  cond_singleton (IntMap.find v cl = ExternalAsyncScript) v;
                nondet =
                  try IntMap.find v has_nondeterminism
                  with Not_found -> StringSet.empty 
              })
         depgraph
  with Not_found ->
    failwith "Not_found in calculate_domination"

type verdict =
    (* Deferable cases *)
    Deferable
  | Deferred
  (* Non-deferable cases *)
  | HasDOMAccess
  | IsInlineScript
  | IsAsyncScript
  | DominatedByDOMAccess
  | DominatedByInlineScript
  | DominatedByAsyncScript
  | Nondeterministic

let verdict_to_string = function
    (* Deferable cases *)
    Deferable -> "deferable"
  | Deferred -> "already deferred"
  (* Non-deferable cases *)
  | HasDOMAccess -> "performs DOM write"
  | IsInlineScript -> "is an inline script"
  | IsAsyncScript -> "is an async script"
  | DominatedByDOMAccess -> "dominated by a DOM-writing script"
  | DominatedByInlineScript -> "dominated by an inline script"
  | DominatedByAsyncScript -> "dominated by an async script"
  | Nondeterministic -> "potentially nondeterministic"
let pp_verdict = Fmt.using verdict_to_string Fmt.string

type result = {
  verdict: verdict;
  nondet: bool;
  data: analysis_result
}
let pp_result pp { verdict; nondet; data } =
  let open Fmt in
    match verdict with
      | DominatedByDOMAccess
      | DominatedByAsyncScript
      | DominatedByInlineScript ->
          pf pp "%a: %a"
            pp_verdict verdict
            pp_analysis_result data
      | _ ->
          pf pp "%a"
            pp_verdict verdict

let deferability_analysis assume_deterministic cl has_nondeterminism dom =
  Logs.debug ~src:!Log.source (fun m -> m "Performing deferability analysis");
  let open ClassifyTask in
    IntMap.filter_map
      (fun v vc ->
         try
           let ve = match vc with
             | ExternalSyncScript ->
                 let { dom_accesses; inline_scripts; async_scripts; nondet } = dom v  in
                   if IntSet.mem v dom_accesses then HasDOMAccess
                   else if not (IntSet.is_empty dom_accesses) then DominatedByDOMAccess
                   else if not (IntSet.is_empty inline_scripts) then DominatedByInlineScript
                   else if not (IntSet.is_empty async_scripts) then DominatedByAsyncScript
                   else if not (StringSet.is_empty nondet || IntSet.mem v assume_deterministic) then Nondeterministic
                   else Deferable
             | ExternalAsyncScript -> IsAsyncScript
             | ExternalDeferScript -> Deferred
             | InlineScript -> IsInlineScript
             | UnclearScript -> Logs.err ~src:!Log.source
                                  (fun m -> m "Script %d has unclear script type, guessing inline" v);
                                IsInlineScript
             | _ -> raise Exit
           in Some { verdict = ve; nondet = IntMap.mem v has_nondeterminism;
                     data = dom v }
         with Exit -> None | Not_found -> None)
      cl

type domination_facts = {
  trace: Trace.trace;
  classification: ClassifyTask.classification IntMap.t;
  has_dom_write : IntSet.t;
  has_nondeterminism : StringSet.t IntMap.t;
  spec : ReadsWrites.event_standalone_spec IntMap.t;
  po : PostAndWaitGraph.PostWaitGraph.t;
  potential_races : ReducedOrderGraph.RaceSet.t;
  script_short_timeouts : int list;
  dependency_graph: Trace.DependencyGraph.t;
  verdicts: result IntMap.t
}
let calculate_domination assume_deterministic trace =
  let open ReducedOrderGraph in
  let { trace; classification; has_dom_write; has_nondeterminism;
        spec; po; potential_races; script_short_timeouts;
        dependency_graph } = ReducedOrderGraph.calculate trace
  in let dom =
    calculate_domination has_nondeterminism has_dom_write
      classification dependency_graph
  in let def = deferability_analysis assume_deterministic
                 classification has_nondeterminism dom
  in { trace; classification; has_dom_write; has_nondeterminism;
       spec; po; potential_races; script_short_timeouts;
       dependency_graph; verdicts = def }

