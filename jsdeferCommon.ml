let with_out_file filename f =
  let chan = open_out filename in try
    f chan;
    close_out chan
  with e ->
    close_out chan;
    raise e

let with_formatter filename f =
  with_out_file filename
    (fun chan ->
       let pp = Format.formatter_of_out_channel chan in
         Format.pp_open_vbox pp 0;
         f pp;
         Format.pp_close_box pp ();
         Format.pp_print_flush pp ())

let write_to_file filename (fmt: 'a Fmt.t) data =
  with_formatter filename (fun pp -> fmt pp data)

let load_determinism_facts filename =
  try
    BatFile.lines_of filename |> BatEnum.fold
      (fun known_det str -> IntSet.add (int_of_string str) known_det)
      IntSet.empty
  with _ -> IntSet.empty

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
  script_verdict: Domination.result;
  has_dom_writes: bool;
  has_potential_nondeterminism: StringSet.t;
  assumed_deterministic: bool;
  potential_races: ReducedOrderGraph.RaceSet.t;
}

let pp_determinism pp = function
  | (nondet, true) when (StringSet.is_empty nondet) ->
      Fmt.string pp "Redundantly marked as deterministic"
  | (nondet, false) when StringSet.is_empty nondet ->
      Fmt.string pp "Deterministic"
  | (nondet, true) ->
      Fmt.pf pp "Explicitly assumed deterministic, in view of the following: %a"
        (Fmt.iter StringSet.iter Fmt.string) nondet
  | (nondet, false) ->
      Fmt.pf pp "Inferred as non-deterministic, in view of the following: %a"
        (Fmt.iter StringSet.iter Fmt.string) nondet

let pp_summary pp { script_provenance; script_verdict; has_dom_writes;
                    has_potential_nondeterminism; assumed_deterministic;
                    potential_races } =
  Fmt.pf pp "%a@,%a@,Has DOM writes: %b@,Non-determinism: %a@,Races: %a@,"
    pp_provenance script_provenance
    Domination.pp_verdict script_verdict.Domination.verdict
    has_dom_writes
    pp_determinism (has_potential_nondeterminism, assumed_deterministic)
    ReducedOrderGraph.pp_races potential_races

let lookup_url trace script =
  let open Trace in
  let commands = IntMap.find script trace
  in let rec find = function
    | Enter (JSCode { source }) :: _ -> source
    | _ :: rest -> find rest
    | [] -> Logs.err (fun k -> k "Can't find URL for script %d, adding dummy" script);
            "<unknown>"
  in find commands

let summarize_one assumed trace cl has_dom_write has_nondeterminism potential_races script result =
  let script_class = IntMap.find script cl in
    assert (ClassifyTask.is_toplevel_script script_class);
    let script_provenance = match script_class with
      | ClassifyTask.InlineScript -> ScriptInline
      | ClassifyTask.ExternalSyncScript -> ScriptSynchronous (lookup_url trace script)
      | ClassifyTask.ExternalAsyncScript -> ScriptAsynchronous (lookup_url trace script)
      | ClassifyTask.ExternalDeferScript -> ScriptDeferred (lookup_url trace script)
      | ClassifyTask.ExternalUnknownScript -> ScriptOther (lookup_url trace script)
      | _ -> assert false
    and script_verdict = result
    and has_dom_writes = IntSet.mem script has_dom_write
    and assumed_deterministic = IntSet.mem script assumed
    and has_potential_nondeterminism = try
      IntMap.find script has_nondeterminism
    with Not_found -> StringSet.empty
    and potential_races =
      ReducedOrderGraph.RaceSet.filter
        (fun { ReducedOrderGraph.script = script' } -> script = script')
        potential_races
    in Logs.info (fun k -> k "Adding script %d" script);
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
  IntMap.fold (fun script { script_provenance; script_verdict } defs ->
                 if script_verdict.Domination.verdict = Domination.Deferable then
                   (script, script_provenance) :: defs
                 else
                   defs)
    per_script []

let summarize base assumed trace cl has_dom_write has_nondeterminism potential_races depgraph result =
  (* Do a per-script summary *)
  let trace_map =
    let open Trace in
    List.fold_left
      (fun trace_map { commands; id } -> IntMap.add id commands trace_map)
      IntMap.empty trace.events
  in let per_script = IntMap.mapi (summarize_one assumed trace_map cl has_dom_write has_nondeterminism potential_races) result
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
      (list ~sep:cut (pair ~sep:(const string "\t") int pp_provenance)) deferables
      (iter_bindings ~sep:cut IntMap.iter pp_script_summary) per_script

let short_str_provenance = function
  | ScriptInline -> "<inline>"
  | ScriptSynchronous src -> "sync " ^ src
  | ScriptAsynchronous src -> "async " ^ src
  | ScriptDeferred src -> "defer " ^ src
  | ScriptOther src -> "other " ^ src
let short_str_verdict = let open Domination in function
  | Deferable -> "deferable"
  | Deferred -> "deferred"
  | HasDOMAccess -> "DOM access"
  | IsInlineScript -> "inline"
  | IsAsyncScript -> "async"
  | DominatedByDOMAccess -> "<- DOM"
  | DominatedByInlineScript -> "<- inline"
  | DominatedByAsyncScript -> "<- async"
  | Nondeterministic -> "nondet"
let pp_short_race pp { ReducedOrderGraph.script_ev; racing_ev; refs } =
  Fmt.pf pp "%d-%d@%a" script_ev racing_ev Trace.pp_reference refs
let make_row name id { script_provenance; script_verdict; has_dom_writes;
                       assumed_deterministic; has_potential_nondeterminism;
                       potential_races } =
  let open Fmt in
  [
    name;
    string_of_int id;
    short_str_provenance script_provenance;
    short_str_verdict script_verdict.Domination.verdict;
    if has_dom_writes then "has DOM writes!" else "-";
    if assumed_deterministic then "assumed deterministic!" else "-";
    strf "@[<h>%a@]" (iter ~sep:sp StringSet.iter string) has_potential_nondeterminism;
    strf "@[<h>%a@]" (iter ~sep:sp ReducedOrderGraph.RaceSet.iter pp_short_race) potential_races
  ]

let csv_page_summary { per_script; name } chan =
  let open Csv in
  output_all (to_channel chan) @@
  IntMap.fold (fun id data rows -> make_row name id data :: rows) per_script []

let re_query_string = Str.regexp "?.*"
let url = function
  | ScriptInline -> "???"
  | ScriptSynchronous s
  | ScriptAsynchronous s
  | ScriptDeferred s
  | ScriptOther s ->
      Str.replace_first re_query_string "?<query string>" s

let pp_defer pp { deferables } =
  let open Fmt in
    vbox (list ~sep:cut (hbox @@ pair ~sep:sp int (using url string)))
      pp deferables

let calculate_and_write_analysis log use_det base intrace indet makeoutput =
  if !log then DetailLog.open_log (open_out (makeoutput ".details"));
  let deterministic_scripts =
    if use_det && Sys.file_exists indet then
      load_determinism_facts indet
    else
      IntSet.empty
  in let { Domination.trace; classification; has_dom_write; has_nondeterminism; potential_races;
           dependency_graph; verdicts } =
    CleanLog.load intrace
      |> Trace.parse_trace
      |> Domination.calculate_domination deterministic_scripts
  in let summary =
    summarize base deterministic_scripts trace classification has_dom_write has_nondeterminism potential_races dependency_graph verdicts
  in
    write_to_file (makeoutput "result") pp_page_summary summary;
    with_out_file (makeoutput "result.csv") (csv_page_summary summary);
    write_to_file (makeoutput "defer") pp_defer summary

let analyze log filename use_determinism_facts =
  let open Filename in
  if Sys.is_directory filename then
    calculate_and_write_analysis log
      use_determinism_facts
      (basename filename)
      (concat filename "ER_actionlog")
      (concat filename "deterministic")
      (fun suffix -> concat filename suffix)
  else
    let base = Filename.chop_suffix (Filename.basename filename) ".log" in
    calculate_and_write_analysis log use_determinism_facts base filename
      (base ^ ".deterministic")
      (fun suffix -> base ^ suffix)


