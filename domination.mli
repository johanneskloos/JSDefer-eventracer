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

val calculate_domination :
  IntSet.t ->
  Trace.trace ->
  Trace.trace * ClassifyTask.classification IntMap.t *
  ReducedOrderGraph.trace_facts * ReducedOrderGraph.trace_facts *
  Trace.value option Trace.ReferenceMap.t * Trace.DependencyGraph.t *
  (int -> analysis_result) * result IntMap.t
