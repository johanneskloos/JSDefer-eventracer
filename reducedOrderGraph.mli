val guid_heuristic : bool ref

type trace_facts = {
  trace: Trace.trace;
  classification: ClassifyTask.classification IntMap.t;
  has_dom_write : IntSet.t;
  has_nondeterminism : StringSet.t IntMap.t;
  spec : ReadsWrites.event_standalone_spec IntMap.t;
  po : PostAndWaitGraph.PostWaitGraph.t;
  potential_races : Races.RaceSet.t;
  script_short_timeouts : int list;
  dependency_graph: Trace.DependencyGraph.t
}
val calculate :
  Trace.trace -> trace_facts
