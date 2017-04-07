(** Task pools. *)

(** [start_task  ?timeout func arg] starts a task
  executing [func arg], optionally with timeout.
  Only a certain number of tasks, as given in
  [Config.task_pool_max], can be running at the same
  time. *)
val start_task : int option -> ('a -> 'b) -> 'a -> unit

(** Wait until all running tasks complete. *)
val drain : unit -> unit
