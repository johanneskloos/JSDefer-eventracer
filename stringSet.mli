(** Sets of strings. *)

include BatSet.S with type elt = string
val pp : ?sep:unit Fmt.t -> t Fmt.t
