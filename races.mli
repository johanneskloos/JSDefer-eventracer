(** Handling races between events. *)

(** Races that involve at least one script. *)
type race = {
  script : int; (** The involved script. *)
  script_ev : int; (** The script-associated event in the race. *)
  racing_ev : int; (** The other event in the race. *)
  refs : Trace.reference; (** The reference on which there is a race. *)
} [@@deriving show, ord]

(** Sets of races. *)
module RaceSet : BatSet.S with type elt = race
val pp_races : RaceSet.t Fmt.t

(** [find_potential_races race_info scripts] filters out the
    races from [race_info] that are relevant for scripts in [scripts]. *)
val find_potential_races : Trace.race_info list -> int list -> RaceSet.t
