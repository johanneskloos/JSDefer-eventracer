(** Propagate deferability information from dominating scripts. *)

(** Per-script propagation results. *)
type analysis_result = {
  dom_accesses : IntSet.t;
  (** Dominating DOM-accessing scripts *)
  inline_scripts : IntSet.t;
  (** Dominating inline scripts. *)
  async_scripts : IntSet.t;
  (** Dominating async scripts. *)
  nondet : StringSet.t;
  (** Non-determinism from dominating scripts. *)
  races_with : Races.RaceSet.t
    (** Race conditions for dominating scripts. *)
}
val pp_analysis_result : analysis_result Fmt.t

(** [calculate_domination nondets doms races classifications deps]
    calculate the above per-script propagation results *)
val calculate_domination: StringSet.t IntMap.t -> IntSet.t ->
  Races.RaceSet.t -> ClassifyTask.trace_classifications ->
  Trace.DependencyGraph.t -> (int -> analysis_result)
