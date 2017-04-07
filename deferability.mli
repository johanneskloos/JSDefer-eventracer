type verdict =
    Deferable
  | Deferred
  | HasDOMAccess
  | IsInlineScript
  | IsAsyncScript
  | DominatedByDOMAccess
  | DominatedByInlineScript
  | DominatedByAsyncScript
  | Nondeterministic
  | Racing
val verdict_to_string : verdict -> string
val pp_verdict : verdict Fmt.t

type result = {
  verdict : verdict;
  data : Domination.analysis_result;
}
val pp_result : result Fmt.t

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
val calculate_deferability :
  IntSet.t ->
  Trace.trace ->
  deferability_facts
