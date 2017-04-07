(** Database back-end. *)

(** [write_to_database filename summary]:
    Write [summary] to the SQLite databse [filename]. *)
val write_to_database : string -> Summary.page_summary -> unit
