(** Maps on strings. *)

include BatMap.S with type key = string
val pp : ?sep:unit Fmt.t -> (key * 'a) Fmt.t -> 'a t Fmt.t
val pp_default :
  ?esep:unit Fmt.t -> ?psep:unit Fmt.t -> 'a Fmt.t -> 'a t Fmt.t
