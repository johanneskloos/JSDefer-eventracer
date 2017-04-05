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

let log = ref false
let use_determinism_facts = ref false

let calculate_and_write_analysis base intrace indet makeoutput =
  if !log then DetailLog.open_log (open_out (makeoutput ".details"));
  let deterministic_scripts =
    if !use_determinism_facts && Sys.file_exists indet then
      load_determinism_facts indet
    else
      IntSet.empty
  in let summary =
    CleanLog.load intrace
    |> Trace.parse_trace
    |> Deferability.calculate_deferability deterministic_scripts
    |> Summary.summarize base deterministic_scripts
  in
    write_to_file (makeoutput "result") Summary.pp_page_summary summary;
    with_out_file (makeoutput "result.csv") (Summary.csv_page_summary summary);
    write_to_file (makeoutput "defer") Summary.pp_defer summary

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

let () =
  let tasks = ref []
  and timeout = ref None in
  Arg.parse [
    ("-G", Arg.Set CalculateRFandMO.guid_heuristic, "GUID heuristic (HACK)");
    ("-L", Arg.Set log, "log file");
    ("-n", Arg.Set_int TaskPool.task_pool_max, "number of parallel tasks");
    ("-t", Arg.Int (fun n -> timeout := Some n), "timeout (in seconds)");
    ("-D", Arg.Unit (fun () -> Logs.set_level ~all:true (Some Logs.Debug)), "enable debugging output");
    ("-T", Arg.Unit (fun () -> timeout := None), "no timeout");
    ("-d", Arg.Set use_determinism_facts, "use information from determinism fact files")
  ] (fun task -> tasks := task :: !tasks) "";
  List.iter (fun fn -> TaskPool.start_task !timeout analyze fn) !tasks;
  TaskPool.drain ();
  DetailLog.close_log ()
