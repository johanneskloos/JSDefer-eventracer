(** Compute longest common subsequences. *)

(** [longest_common_subsequence equal merge l1 l2]
  computes the longest common subsequence of [l1] and [l2],
  as indicated by [equal], and uses [merge] to merge matching elements
  of [l1] and [l2].

  [longest_common_subsequence (=) (fun x _ -> x) l1 l2] is the
  classical usage of this algorithm, but it also allows
  for heterogenous input lists. *)
val longest_common_subsequence :
  ('a -> 'b -> bool) -> ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
