(** Calculate specifications for all events in a trace. *)

(** Specification of an event. *)
type event_spec = {
  reads : Trace.value option Trace.ReferenceMap.t;
  (** References that have been read without prior write, with values. *)
  writes : Trace.value option Trace.ReferenceMap.t;
  (** References that have been written and not overwritten, with values. *)
  posts : IntSet.t;
(** Tasks that have been posted. *)
}

(** Specification of all events. *)
type trace_specs = event_spec IntMap.t

(** Calculate the specifications of the events in a given trace. *)
val per_event_specification : Trace.trace -> trace_specs

(** Combine two specifications by sequential composition. *)
val combine_reads_writes : event_spec -> event_spec -> event_spec
