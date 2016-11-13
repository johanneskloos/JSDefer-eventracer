let task_pool_size = ref 0
let task_pool_max = ref 4

let start_task timeout f x =
  while !task_pool_size >= !task_pool_max do
    ignore (Unix.wait ())
  done;
  match Unix.fork () with
    | 0 ->
        begin
          match timeout with
            | Some t ->
                Sys.set_signal Sys.sigalrm (Sys.Signal_handle exit);
                ignore (Unix.alarm t)
            | None -> ()
        end;
        f x;
        exit 0
    | pid -> incr task_pool_size

let drain () =
  while !task_pool_size > 0 do
    ignore (Unix.wait ())
  done

let chldhandler (_: int) = decr task_pool_size

let run_analysis log file =
  Log.set_source_for_file file;
  JsdeferCommon.analyze log file

let improved_reporter () =
  let report (src: Logs.src) (level: Logs.level) ~over k msgf =
    let k _ = over (); k () in
      msgf @@ fun ?header ?tags fmt ->
      Format.kfprintf k Fmt.stdout ("%a %s: @[" ^^ fmt ^^ "@]@.")
        Logs.pp_header (level, header)
        (Logs.Src.name src)
  in { Logs.report }

let () =
  Logs.set_reporter (improved_reporter ());
  Logs.set_level ~all:true (Some Logs.Info);
  let log = ref false
  and tasks = ref []
  and timeout = ref None in
  Arg.parse [
    ("-G", Arg.Set OrderGraph.guid_heuristic, "GUID heuristic (HACK)");
    ("-L", Arg.Set log, "log file");
    ("-n", Arg.Set_int task_pool_max, "number of parallel tasks");
    ("-t", Arg.Int (fun n -> timeout := Some n), "timeout (in seconds)");
    ("-D", Arg.Unit (fun () -> Logs.set_level ~all:true (Some Logs.Debug)), "enable debugging output");
    ("-T", Arg.Unit (fun () -> timeout := None), "no timeout")
  ] (fun task -> tasks := task :: !tasks) "";
  Sys.set_signal Sys.sigchld (Sys.Signal_handle chldhandler);
  List.iter (fun fn -> start_task !timeout (run_analysis log) fn) !tasks;
  drain ()
