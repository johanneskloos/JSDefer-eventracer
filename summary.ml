type provenance =
  | ScriptInline
  | ScriptSynchronous of string
  | ScriptAsynchronous of string
  | ScriptDeferred of string
  | ScriptOther of string
let str_provenance = function
  | ScriptInline -> "inline script"
  | ScriptSynchronous src -> "synchronous script from `" ^ src ^ "'"
  | ScriptAsynchronous src -> "asynchronous script from `" ^ src ^ "'"
  | ScriptDeferred src -> "deferred script from `" ^ src ^ "'"
  | ScriptOther src -> "other kind script from `" ^ src ^ "'"
let pp_provenance = Fmt.using str_provenance Fmt.string

type summary = {
  script_provenance: provenance;
  script_verdict: Deferability.verdict;
  has_dom_writes: bool;
  has_potential_nondeterminism: StringSet.t;
  assumed_deterministic: bool;
  potential_races: Races.RaceSet.t;
}

let pp_determinism pp = function
  | (nondet, true) when (StringSet.is_empty nondet) ->
      Fmt.string pp "Redundantly marked as deterministic"
  | (nondet, false) when StringSet.is_empty nondet ->
      Fmt.string pp "Deterministic"
  | (nondet, true) ->
      Fmt.pf pp
        "Explicitly assumed deterministic, in view of the following: %a"
        (Fmt.iter StringSet.iter Fmt.string) nondet
  | (nondet, false) ->
      Fmt.pf pp
        "Inferred as non-deterministic, in view of the following: %a"
        (Fmt.iter StringSet.iter Fmt.string) nondet

let pp_summary pp { script_provenance; script_verdict; has_dom_writes;
                    has_potential_nondeterminism; assumed_deterministic;
                    potential_races } =
  Fmt.pf pp "%a@,%a@,Has DOM writes: %b@,Non-determinism: %a@,Races: %a@,"
    pp_provenance script_provenance
    Deferability.pp_verdict script_verdict
    has_dom_writes
    pp_determinism (has_potential_nondeterminism, assumed_deterministic)
    Races.pp_races potential_races

let lookup_url trace script =
  let open Trace in
  let commands = IntMap.find script trace
  in let rec find = function
    | Enter (JSCode { source }) :: _ -> source
    | _ :: rest -> find rest
    | [] -> Log.err
              (fun k -> k "Can't find URL for script %d, adding dummy" script);
            "<unknown>"
  in find commands

let get_provenance trace script = let open ClassifyTask in function
  | InlineScript -> ScriptInline
  | ExternalSyncScript -> ScriptSynchronous (lookup_url trace script)
  | ExternalAsyncScript -> ScriptAsynchronous (lookup_url trace script)
  | ExternalDeferScript -> ScriptDeferred (lookup_url trace script)
  | ExternalUnknownScript -> ScriptOther (lookup_url trace script)
  | _ -> assert false
let summarize_one assumed trace cl script { Deferability.verdict; data } =
  let script_class = IntMap.find script cl in
    assert (ClassifyTask.is_toplevel_script script_class);
    let script_provenance = get_provenance  trace script script_class
    and script_verdict = verdict
    and has_dom_writes = not (IntSet.is_empty data.Domination.dom_accesses)
    and assumed_deterministic = IntSet.mem script assumed
    and has_potential_nondeterminism = data.Domination.nondet
    and potential_races = data.Domination.races_with
    in Log.info (fun k -> k "Adding script %d" script);
       { script_provenance; script_verdict;
         has_dom_writes; assumed_deterministic;
         has_potential_nondeterminism;
         potential_races }

type page_summary = {
  per_script: summary IntMap.t;
  name: string;
  deferables: (int * provenance) list
}

let calculate_deferables per_script =
  let open Deferability in
  IntMap.fold (fun script { script_provenance; script_verdict } defs ->
                 if script_verdict = Deferable then
                   (script, script_provenance) :: defs
                 else
                   defs)
    per_script []

let summarize base assumed
      { Deferability.trace; classification; has_dom_write;
        has_nondeterminism; potential_races; verdicts } =
  (* Do a per-script summary *)
  let trace_map =
    let open Trace in
      List.fold_left
        (fun trace_map { commands; id } -> IntMap.add id commands trace_map)
        IntMap.empty trace.events
  in let per_script =
    IntMap.mapi
      (summarize_one assumed trace_map classification) verdicts
  in { per_script; name = base; deferables = calculate_deferables per_script }

let pp_script_summary pp (script, summary) =
  Fmt.pf pp "@,Script %d:@,%a@," script pp_summary summary
let pp_page_summary pp { per_script; name; deferables } =
  let open Fmt in
    pf pp ("@[<v>Deferability report for %s@,@," ^^
           "Number of scripts: %d@," ^^
           "Deferable scripts on the page:@," ^^
           "%a@,@," ^^
           "Per-script analysis:" ^^
           "%a@.")
      name
      (IntMap.cardinal per_script)
      (list ~sep:cut (pair ~sep:(const string "\t") int pp_provenance))
      deferables
      (iter_bindings ~sep:cut IntMap.iter pp_script_summary) per_script

let short_str_provenance = function
  | ScriptInline -> "<inline>"
  | ScriptSynchronous src -> "sync " ^ src
  | ScriptAsynchronous src -> "async " ^ src
  | ScriptDeferred src -> "defer " ^ src
  | ScriptOther src -> "other " ^ src
let short_str_provenance_type = function
  | ScriptInline -> "inline"
  | ScriptSynchronous _ -> "sync"
  | ScriptAsynchronous _ -> "async"
  | ScriptDeferred _ -> "defer"
  | ScriptOther _ -> "other"
let str_provenance_source = function
  | ScriptInline -> "<inline>"
  | ScriptSynchronous src
  | ScriptAsynchronous src
  | ScriptDeferred src
  | ScriptOther src -> src
let short_str_verdict = let open Deferability in function
  | Deferable -> "deferable"
  | Deferred -> "deferred"
  | HasDOMAccess -> "DOM access"
  | IsInlineScript -> "inline"
  | IsAsyncScript -> "async"
  | DominatedByDOMAccess -> "<- DOM"
  | DominatedByInlineScript -> "<- inline"
  | DominatedByAsyncScript -> "<- async"
  | Nondeterministic -> "nondet"
  | Racing -> "racy"
let re_query_string = Str.regexp "?.*"
let short_str_provenance_source s =
  Str.replace_first re_query_string "?<query string>" (str_provenance_source s)

let pp_short_race pp { Races.script_ev; racing_ev; refs } =
  Fmt.pf pp "%d-%d@%a" script_ev racing_ev Trace.pp_reference refs
let make_row name id { script_provenance; script_verdict; has_dom_writes;
                       assumed_deterministic; has_potential_nondeterminism;
                       potential_races } =
  let open Fmt in
    [
      name;
      string_of_int id;
      short_str_provenance_type script_provenance;
      short_str_provenance_source script_provenance;
      short_str_verdict script_verdict;
      if has_dom_writes then "has DOM writes!" else "-";
      if assumed_deterministic then "assumed deterministic!" else "-";
      strf "@[<h>%a@]" (iter ~sep:sp StringSet.iter string)
        has_potential_nondeterminism;
      strf "@[<h>%a@]" (iter ~sep:sp Races.RaceSet.iter pp_short_race)
        potential_races;
      str_provenance_source script_provenance;
    ]

let csv_page_summary { per_script; name } chan =
  let open Csv in
    output_all (to_channel chan) @@
    IntMap.fold (fun id data rows -> make_row name id data :: rows)
      per_script []

let url = function
  | ScriptInline -> "???"
  | ScriptSynchronous s
  | ScriptAsynchronous s
  | ScriptDeferred s
  | ScriptOther s -> s

let pp_defer pp { deferables } =
  let open Fmt in
    vbox (list ~sep:cut (hbox @@ pair ~sep:sp int (using url string)))
      pp deferables

