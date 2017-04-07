(** Calculate inter-event ordering dependencies. *)

(** Given a specification of all the events in a trace,
    calculate the joint RF and MO order between the events. *)
val calculate_dependency_graph :
  ReadsWrites.trace_specs -> Trace.DependencyGraph.t
