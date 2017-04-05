type event_standalone_spec = {
  reads : Trace.value option Trace.ReferenceMap.t;
  writes : Trace.value option Trace.ReferenceMap.t;
  posts : IntSet.t;
}
val per_event_specification : Trace.trace -> event_standalone_spec IntMap.t
val combine_reads_writes :
  event_standalone_spec -> event_standalone_spec -> event_standalone_spec
