(** Analyze deferability of a page. *)

(** Verdict for a script. *)
type verdict =
    Deferable (** Loaded synchronously, but can be loaded [defer] *)
  | Deferred (** Loaded defer. *)
  | HasDOMAccess (** Cannot be loaded defer: accesses the DOM *)
  | IsInlineScript (** Cannot be loaded defer: is an inline script *)
  | IsAsyncScript (** Cannot be loaded defer: is an [async] script *)
  | DominatedByDOMAccess
  (** Cannot be loaded defer: dominated by a DOM-accessing script. *)
  | DominatedByInlineScript
  (** Cannot be loaded defer: dominated by an inline script. *)
  | DominatedByAsyncScript
  (** Cannot be loaded defer: dominated by an [async] script. *)
  | Nondeterministic
  (** Cannot be loaded defer: script or dominator is non-deterministic. *)
  | Racing
(** Cannot be loaded defer: script or dominator is racy. *)

val verdict_to_string : verdict -> string
val pp_verdict : verdict Fmt.t

(** Per-script analysis result. *)
type result = {
  verdict : verdict;
  data : Domination.analysis_result;
}
val pp_result : result Fmt.t

(** All the facts inferred during deferability analysis. *)
type deferability_facts = {
  trace: Trace.trace; (** The trace. *)
  classification: ClassifyTask.trace_classifications;
  (** The script classifications *)
  has_dom_write : IntSet.t;
  (** Set of DOM-writing scripts *)
  has_nondeterminism : StringSet.t IntMap.t;
  (** Set of non-deterministic scripts, with tags *)
  spec : ReadsWrites.trace_specs;
  (** Script specifications *)
  po : PostAndWaitGraph.PostWaitGraph.t;
  (** Program order *)
  potential_races : Races.RaceSet.t;
  (** Races involving relevant event *)
  script_short_timeouts : int list;
  (** Scripts with short timeouts *)
  dependency_graph: Trace.DependencyGraph.t;
  (** The script dependency graph *)
  verdicts: result IntMap.t
(** Deferability analysis verdicts *)
}

(** [calculate_deferability assume_deterministic trace]
  calculates deferability information for [trace],
  under the assumption that the events in
  [assume_deterministic] are, in fact, deterministic. *)
val calculate_deferability :
  IntSet.t ->
  Trace.trace ->
  deferability_facts
