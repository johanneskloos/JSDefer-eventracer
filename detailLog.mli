(** Detailed logging trace file. *)

(** Use the given channel for detailed logging. *)
val open_log : out_channel -> unit

(** Close the detailed log. *)
val close_log : unit -> unit

(** Log to the detailed log file. *)
val log : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
