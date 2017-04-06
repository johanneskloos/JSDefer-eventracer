type analysis_result = {
  dom_accesses : IntSet.t;
  inline_scripts : IntSet.t;
  async_scripts : IntSet.t;
  nondet : StringSet.t;
  races_with : Races.RaceSet.t
}
val pp_analysis_result : analysis_result Fmt.t
val calculate_domination: StringSet.t IntMap.t -> IntSet.t ->
  Races.RaceSet.t -> ClassifyTask.classification IntMap.t ->
  Trace.DependencyGraph.t -> (int -> analysis_result)
