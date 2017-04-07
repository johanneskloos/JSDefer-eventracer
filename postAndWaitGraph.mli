module PostWaitEdge : sig type t = POST | HB [@@deriving ord] end
module PostWaitGraph : Graph.Sig.P with
        type V.t = int
        and type E.t = int * PostWaitEdge.t * int
val build_post_wait_graph :
  Trace.trace -> ClassifyTask.trace_classifications -> PostWaitGraph.t
