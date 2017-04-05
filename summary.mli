type provenance =
    ScriptInline
  | ScriptSynchronous of string
  | ScriptAsynchronous of string
  | ScriptDeferred of string
  | ScriptOther of string
val pp_provenance : provenance Fmt.t

type summary = {
  script_provenance : provenance;
  script_verdict : Deferability.result;
  has_dom_writes : bool;
  has_potential_nondeterminism : StringSet.t;
  assumed_deterministic : bool;
  potential_races : Races.RaceSet.t;
}
val pp_summary : Format.formatter -> summary -> unit

type page_summary = {
  per_script : summary IntMap.t;
  name : string;
  deferables : (int * provenance) list;
}
val pp_page_summary : page_summary Fmt.t
val pp_defer : page_summary Fmt.t
val csv_page_summary : page_summary -> out_channel -> unit

val summarize : string -> IntSet.t ->
  Deferability.deferability_facts -> page_summary
