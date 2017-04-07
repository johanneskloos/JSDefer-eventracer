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
  | Racing

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
  | Racing -> "involved in a race condition"
let pp_verdict = Fmt.using verdict_to_string Fmt.string

type result = {
  verdict: verdict;
  data: Domination.analysis_result
}
let pp_result pp { verdict; data } =
  let open Fmt in
    match verdict with
      | DominatedByDOMAccess
      | DominatedByAsyncScript
      | DominatedByInlineScript ->
          pf pp "%a: %a"
            pp_verdict verdict
            Domination.pp_analysis_result data
      | _ ->
          pf pp "%a"
            pp_verdict verdict

let classify_script assume_deterministic v
      { Domination.dom_accesses; inline_scripts; async_scripts; races_with; nondet } =
  if IntSet.mem v dom_accesses then HasDOMAccess
  else if not (IntSet.is_empty dom_accesses) then DominatedByDOMAccess
  else if not (IntSet.is_empty inline_scripts) then DominatedByInlineScript
  else if not (IntSet.is_empty async_scripts) then DominatedByAsyncScript
  else if not (Races.RaceSet.is_empty races_with) then Racing
  else if not (StringSet.is_empty nondet || IntSet.mem v assume_deterministic)
  then Nondeterministic
  else Deferable

let deferability_analysis assume_deterministic cl has_nondeterminism dom =
  Log.debug (fun m -> m "Performing deferability analysis");
  let open ClassifyTask in
    IntMap.filter_map
      (fun v vc ->
         try
           let ve = match vc with
             | ExternalSyncScript ->
                 classify_script assume_deterministic v (dom v)
             | ExternalAsyncScript -> IsAsyncScript
             | ExternalDeferScript -> Deferred
             | InlineScript -> IsInlineScript
             | UnclearScript ->
                 Log.err
                   (fun m -> m ("Script %d has unclear script type," ^^
                                "guessing inline") v);
                 IsInlineScript
             | _ -> raise Exit
           in Some { verdict = ve; data = dom v }
         with Exit -> None | Not_found -> None)
      cl

type deferability_facts = {
  trace: Trace.trace;
  classification: ClassifyTask.trace_classifications;
  has_dom_write : IntSet.t;
  has_nondeterminism : StringSet.t IntMap.t;
  spec : ReadsWrites.trace_specs;
  po : PostAndWaitGraph.PostWaitGraph.t;
  potential_races : Races.RaceSet.t;
  script_short_timeouts : int list;
  dependency_graph: Trace.DependencyGraph.t;
  verdicts: result IntMap.t
}
let calculate_deferability assume_deterministic trace =
  let open ReducedOrderGraph in
  let { trace; classification; has_dom_write; has_nondeterminism;
        spec; po; potential_races; script_short_timeouts;
        dependency_graph } = ReducedOrderGraph.calculate trace
  in let dom =
    Domination.calculate_domination has_nondeterminism has_dom_write
      potential_races classification dependency_graph
  in let def = deferability_analysis assume_deterministic
                 classification has_nondeterminism dom
  in { trace; classification; has_dom_write; has_nondeterminism;
       spec; po; potential_races; script_short_timeouts;
       dependency_graph; verdicts = def }

