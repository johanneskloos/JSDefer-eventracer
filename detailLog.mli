val open_log : out_channel -> unit
val close_log : unit -> unit
val log : ((('a, Format.formatter, unit) format -> 'a) -> unit) -> unit
