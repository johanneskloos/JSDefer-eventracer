val guid_heuristic : bool ref

type race = {
  script : int;
  script_ev : int;
  racing_ev : int;
  refs : Trace.reference;
} [@@deriving ord, show]
module RaceSet : BatSet.S with type elt = race
val pp_races : RaceSet.t Fmt.t

type trace_facts = {
  has_dom_write : IntSet.t;
  has_nondeterminism : StringSet.t IntMap.t;
  spec : ReadsWrites.event_standalone_spec IntMap.t;
  po : PostAndWaitGraph.PostWaitGraph.t;
  potential_races : RaceSet.t;
  script_short_timeouts : int list;
}
val calculate :
  Trace.trace ->
  Trace.trace * ClassifyTask.classification IntMap.t * trace_facts * trace_facts *
  Trace.value option Trace.ReferenceMap.t * Trace.DependencyGraph.t
