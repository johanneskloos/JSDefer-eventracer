val guid_heuristic: bool ref
val calculate_dependency_graph :
  ReadsWrites.event_standalone_spec IntMap.t -> Trace.DependencyGraph.t
