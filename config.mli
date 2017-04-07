(** Configuration parameters. *)

(** Whether to write a detailed log file for each analysis. *)
val use_detailed_log : bool ref

(** Whether to make use of determinism fact files. *)
val use_determinism_facts : bool ref

(** Use the GUID heuristic for jQuery. *)
val guid_heuristic : bool ref

(** Maximal size of the task pool. *)
val task_pool_max : int ref
