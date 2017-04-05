type race = {
  script : int;
  script_ev : int;
  racing_ev : int;
  refs : Trace.reference;
} [@@deriving show, ord]
module RaceSet : BatSet.S with type elt = race
val pp_races : RaceSet.t Fmt.t
val find_potential_races : Trace.race_info list -> int list -> RaceSet.t
