let task_pool_size = ref 0

let start_task timeout f x =
  while !task_pool_size >= !Config.task_pool_max do
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

let () =
  Sys.set_signal Sys.sigchld (Sys.Signal_handle chldhandler);

