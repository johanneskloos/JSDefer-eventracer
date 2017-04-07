type event_spec = {
  reads : Trace.value option Trace.ReferenceMap.t;
  writes : Trace.value option Trace.ReferenceMap.t;
  posts : IntSet.t;
}
type trace_specs = event_spec IntMap.t
val per_event_specification : Trace.trace -> trace_specs
val combine_reads_writes : event_spec -> event_spec -> event_spec
