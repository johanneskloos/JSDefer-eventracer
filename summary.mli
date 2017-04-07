(** Summarize deferability information. *)

(** Provenance of a script. The cases with a [string] parameter
    contain the script's URL. *)
type provenance =
    ScriptInline
  | ScriptSynchronous of string
  | ScriptAsynchronous of string
  | ScriptDeferred of string
  | ScriptOther of string
val pp_provenance : provenance Fmt.t

(** Per-script summary of the analysis. *)
type summary = {
  script_provenance : provenance;
  script_verdict : Deferability.verdict;
  has_dom_writes : bool;
  has_potential_nondeterminism : StringSet.t;
  assumed_deterministic : bool;
  potential_races : Races.RaceSet.t;
  (** Races involving this script. *)
}
val pp_summary : Format.formatter -> summary -> unit

(** Analysis summary for the whole page. *)
type page_summary = {
  per_script : summary IntMap.t; (** Per-script information *)
  name : string; (** Page URL *)
  deferables : (int * provenance) list;
   (* List of deferable scripts, containing the script's
      first execution event identifier and its provenance. *)
}

(** Pretty-print the page summary, in user-readable form. *)
val pp_page_summary : page_summary Fmt.t

(** Pretty-print the deferability information. *)
val pp_defer : page_summary Fmt.t

(** Write out the per-script analysis as a CSV file. *)
val csv_page_summary : page_summary -> out_channel -> unit

(** [summarize url assumed facts] summarizes the analysis
   results for page [url], as given in [facts], and [assumed]
  contains the script identifiers for scripts assumed
  deterministic. *)
val summarize : string -> IntSet.t ->
  Deferability.deferability_facts -> page_summary
