open ClassifyTask
open Trace
open ReducedOrderGraph
open ClassificationLayout
open Domination

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

let pp_trace_with_classification cl pp { id; evtype; commands } =
  let open Fmt in
    pf pp "@[<v2>Event %d: %a, %a {@,%a@]@,}"
      id
      pp_event_action_type evtype
      pp_classification (try IntMap.find id cl with Not_found -> failwith "missing cl")
      (list ~sep:cut pp_command) commands

let write_classified_trace filename { events } cl =
  write_to_file filename
    Fmt.(list ~sep:cut (pp_trace_with_classification cl)) events

let pp_access =
  let open Fmt in
  ReferenceMap.pp_default ~esep:sp ~psep:(const string "=")
    (option ~none:(const string "(?)") Trace.pp_value)

let pp_spec pp { ReadsWrites.reads; writes; posts } =
  let open Fmt in
    pf pp "Reads: @[<hov>%a@]@,Writes: @[<hov>%a@]@,Posts: @[<hov>%a@]@,"
      pp_access reads
      pp_access writes
      (IntSet.pp ~sep:sp) posts

let pp_data pp { ReducedOrderGraph.has_dom_write; has_nondeterminism;
                 spec; po; potential_races; script_short_timeouts } =
  let open Fmt in
    pf pp "@[<v>Events with DOM writes: @[<hov>%a@]@,\
                Events with nondeterminism: @[<hov>%a@]@,\
                Potential races: @[<hov>%a@]@,\
                Short-timeout events caused by scripts: @[<hov>%a@]@,\
                Specifications:@,%a@]@."
      (IntSet.pp ~sep:sp) has_dom_write
      (IntMap.pp_default ~esep:sp ~psep:(const string ":")
         (braces (StringSet.pp ~sep:sp))) has_nondeterminism
      pp_races potential_races
      (list ~sep:sp int) script_short_timeouts
      (IntMap.pp ~sep:cut (vbox ~indent:2 (pair ~sep:sp int pp_spec))) spec


let write_data filename = write_to_file filename pp_data
      
let write_pre filename dcl_pre =
  let open Fmt in
    write_to_file filename
      (ReferenceMap.pp_default ~psep:(const string ":") (option Trace.pp_value)) dcl_pre

let write_json def chan =
  let open Yojson.Basic in
  let intset_to_list s =
    `List (IntSet.fold (fun i l -> `Int i :: l) s [])
  and stringset_to_list s =
    `List (StringSet.fold (fun i l -> `String i :: l) s [])
  in let data =
    `Assoc (IntMap.fold (fun id { verdict;
                                  data = { dom_accesses; inline_scripts;
                                           async_scripts; nondet } } l ->
                           let data = 
                             `Assoc [
                               ("nondet", `Bool (not (StringSet.is_empty nondet)));
                               ("nondet_causes", stringset_to_list nondet);
                               ("verdict", `String (verdict_to_string verdict));
                               ("domdom", intset_to_list dom_accesses);
                               ("dominline", intset_to_list inline_scripts);
                               ("domasync", intset_to_list async_scripts)
                             ] 
                           in (string_of_int id, data) :: l)
              def [])
  in to_channel chan data

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
  potential_races: RaceSet.t;
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
    pp_verdict script_verdict.verdict
    has_dom_writes
    pp_determinism (has_potential_nondeterminism, assumed_deterministic)
    pp_races potential_races

let lookup_url { events } script =
  let { commands } = BatList.nth events script
  in let rec find = function
    | Enter (JSCode { source }) :: _ -> source
    | _ :: rest -> find rest
    | [] -> Logs.err (fun k -> k "Can't find URL for script %d, adding dummy" script);
            "<unknown>"
  in find commands

let summarize_one assumed trace cl data script result =
  let script_class = IntMap.find script cl in
    assert (is_toplevel_script script_class);
    let script_provenance = match script_class with
      | InlineScript -> ScriptInline
      | ExternalSyncScript -> ScriptSynchronous (lookup_url trace script)
      | ExternalAsyncScript -> ScriptAsynchronous (lookup_url trace script)
      | ExternalDeferScript -> ScriptDeferred (lookup_url trace script)
      | ExternalUnknownScript -> ScriptOther (lookup_url trace script)
      | _ -> assert false
    and script_verdict = result
    and has_dom_writes = IntSet.mem script data.has_dom_write
    and assumed_deterministic = IntSet.mem script assumed
    and has_potential_nondeterminism = try
      IntMap.find script data.has_nondeterminism
    with Not_found -> StringSet.empty
    and potential_races =
      RaceSet.filter (fun { script = script' } -> script = script')
        data.potential_races
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
                 if script_verdict.verdict = Deferable then
                   (script, script_provenance) :: defs
                 else
                   defs)
    per_script []

let summarize base assumed trace cl data depgraph result =
  (* Do a per-script summary *)
  let per_script = IntMap.mapi (summarize_one assumed trace cl data) result
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

let calculate_and_write_analysis log use_det base intrace indet makeoutput =
  if !log then ReducedOrderGraph.log (open_out (makeoutput ".details"));
  let deterministic_scripts =
    if use_det && Sys.file_exists indet then
      load_determinism_facts indet
    else
      IntSet.empty
  in let (trace, cl, data, data', dcl_pre, depgraph, dom, def) =
    CleanLog.load intrace
      |> Trace.parse_trace
      |> Domination.calculate_domination deterministic_scripts
  in let summary =
    summarize base deterministic_scripts trace cl data depgraph def
  in
    write_to_file (makeoutput "result") pp_page_summary summary

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


