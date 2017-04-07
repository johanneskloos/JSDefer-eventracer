(** Logging. *)

(** Set the logging source. *)
val set_source_for_file : string -> unit

(** Write a warning message. *)
val warn: 'a Logs.log

(** Write an error message. *)
val err: 'a Logs.log

(** Write a debugging message. *)
val debug: 'a Logs.log

(** Write an informational message. *)
val info: 'a Logs.log
