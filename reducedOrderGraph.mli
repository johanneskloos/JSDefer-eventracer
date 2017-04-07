(** Calculate the dependency graph used for deferability analysis. *)

(** All the facts computed about the trace so far. *)
type trace_facts = {
  trace: Trace.trace;
  classification: ClassifyTask.trace_classifications;
  has_dom_write : IntSet.t;
  has_nondeterminism : StringSet.t IntMap.t;
  spec : ReadsWrites.trace_specs;
  po : PostAndWaitGraph.PostWaitGraph.t;
  potential_races : Races.RaceSet.t;
  script_short_timeouts : int list;
  dependency_graph: Trace.DependencyGraph.t
}

(** Given a trace, calculate the dependency graph. *)
val calculate :
  Trace.trace -> trace_facts
