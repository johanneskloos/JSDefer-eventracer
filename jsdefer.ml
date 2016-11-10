let task_pool_size = ref 0
let task_pool_max = ref 4

let start_task f x =
  while !task_pool_size >= !task_pool_max do
    ignore (Unix.wait ())
  done;
  match Unix.fork () with
    | 0 -> f x; exit 0
    | pid -> incr task_pool_size

let drain () =
  while !task_pool_size > 0 do
    ignore (Unix.wait ())
  done

let chldhandler (_: int) = decr task_pool_size

let () =
  let log = ref false
  and tasks = ref [] in
  Arg.parse [
    ("-G", Arg.Set OrderGraph.guid_heuristic, "GUID heuristic (HACK)");
    ("-L", Arg.Set log, "log file");
    ("-t", Arg.Set_int task_pool_max, "number of parallel tasks")
  ] (fun task -> tasks := task :: !tasks) "";
  Sys.set_signal Sys.sigchld (Sys.Signal_handle chldhandler);
  List.iter (fun fn -> start_task (JsdeferCommon.analyze log) fn) !tasks;
  drain ()
