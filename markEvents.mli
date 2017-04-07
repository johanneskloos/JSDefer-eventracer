(** Find DOM writes and non-determinism. *)

(** Marking information about events with
    DOM writes and non-determinism. *)
type markings = {
  has_dom_write : IntSet.t;
  (** Script IDs for DOM-writing scripts. *)
  has_nondeterminism : StringSet.t IntMap.t;
  (** Scripts that perform non-deterministic operations,
      with non-determinism tags. *)
}

(** Given a trace, calculate DOM-writing and non-deterministic scripts. *)
val calculate_markings : Trace.trace -> markings
