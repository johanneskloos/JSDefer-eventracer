(** Perform deferability analysis on event traces. *)

(** This is the top-level driver for the analysis;
   see jsdefer --help for information. *)

(**/**)
let load_determinism_facts filename =
  try
    BatFile.lines_of filename |> BatEnum.fold
      (fun known_det str -> IntSet.add (int_of_string str) known_det)
      IntSet.empty
  with _ -> IntSet.empty

let calculate_and_write_analysis base intrace indet makeoutput =
  if !Config.use_detailed_log then
    DetailLog.open_log (open_out (makeoutput "details"));
  let deterministic_scripts =
    if !Config.use_determinism_facts && Sys.file_exists indet then
      load_determinism_facts indet
    else
      IntSet.empty
  in let summary =
    CleanLog.load intrace
    |> Trace.parse_trace
    |> Deferability.calculate_deferability deterministic_scripts
    |> Summary.summarize base deterministic_scripts
  in
    Helpers.write_to_file (makeoutput "result")
      Summary.pp_page_summary summary;
    Helpers.with_out_file (makeoutput "result.csv")
      (Summary.csv_page_summary summary);
    Helpers.write_to_file (makeoutput "defer")
      Summary.pp_defer summary;
    DetailLog.close_log ()

let analyze filename =
  Log.set_source_for_file filename;
  let open Filename in
    if Sys.is_directory filename then
      calculate_and_write_analysis
        (basename filename)
        (concat filename "ER_actionlog")
        (concat filename "deterministic")
        (fun suffix -> concat filename suffix)
    else
      let base = Filename.chop_suffix (Filename.basename filename) ".log" in
        calculate_and_write_analysis
          base
          filename
          (base ^ ".deterministic")
          (fun suffix -> base ^ suffix)

let usage = {|
jsdefer [-dGLDT] [-n max_tasks] [-t timeout] inputs

Calculates deferability information for the traces given in inputs.
For each input, there are possible cases:
- If the input is a file called "<trace>.log", output goes to
  files called "<trace>.<something>".
- If the input is a directory "<dir>" containing a file "ER_actionlog",
  output goes to files called "<dir>/<something>".

The following files are produces:
1. "result", containing a human-readable summary of the analysis.
2. "result.csv", containing the summary as a CSV file.
3. "defer", containg the IDs and URLs of deferable scripts.

If the -L option is given, a file "details" with minute trace information
is produced.

If the -d option is given, an additional file "deterministic" is read,
if it exists, which contains the IDs of scripts to treat as actually
deterministic.

The -t, -T and -n options control how many analysis tasks can run in
parallel, and when they time out.
|}

let () =
  let tasks = ref []
  and timeout = ref None in
  Arg.parse [
    ("-G", Arg.Set Config.guid_heuristic, "GUID heuristic (HACK)");
    ("-L", Arg.Set Config.use_detailed_log, "write detailed log file");
    ("-n", Arg.Set_int Config.task_pool_max, "number of parallel tasks");
    ("-t", Arg.Int (fun n -> timeout := Some n), "timeout (in seconds)");
    ("-D", Arg.Unit (fun () -> Logs.set_level ~all:true (Some Logs.Debug)),
     "enable debugging output");
    ("-T", Arg.Unit (fun () -> timeout := None), "no timeout");
    ("-d", Arg.Set Config.use_determinism_facts,
     "use information from determinism fact files");
  ] (fun task -> tasks := task :: !tasks) usage;
  List.iter (fun fn -> TaskPool.start_task !timeout analyze fn) !tasks;
  TaskPool.drain ()
