(** Calculate program order. *)

(** Types of program order edges. *)
module PostWaitEdge : sig type t = POST | HB [@@deriving ord] end

(** The program order graph. *)
module PostWaitGraph : Graph.Sig.P with
        type V.t = int
        and type E.t = int * PostWaitEdge.t * int

(** Build the program order graph for scripts. *)
val build_post_wait_graph :
  Trace.trace -> ClassifyTask.trace_classifications -> PostWaitGraph.t
