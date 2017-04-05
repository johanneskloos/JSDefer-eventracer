type analysis_result = {
  dom_accesses : IntSet.t;
  inline_scripts : IntSet.t;
  async_scripts : IntSet.t;
  nondet : StringSet.t;
}
val pp_analysis_result : analysis_result Fmt.t

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
val verdict_to_string : verdict -> string
val pp_verdict : verdict Fmt.t

type result = { verdict : verdict; nondet : bool; data : analysis_result; }
val pp_result : result Fmt.t

type domination_facts = {
  trace: Trace.trace;
  classification: ClassifyTask.classification IntMap.t;
  has_dom_write : IntSet.t;
  has_nondeterminism : StringSet.t IntMap.t;
  spec : ReadsWrites.event_standalone_spec IntMap.t;
  po : PostAndWaitGraph.PostWaitGraph.t;
  potential_races : Races.RaceSet.t;
  script_short_timeouts : int list;
  dependency_graph: Trace.DependencyGraph.t;
  verdicts: result IntMap.t
}
val calculate_domination :
  IntSet.t ->
  Trace.trace ->
  domination_facts
