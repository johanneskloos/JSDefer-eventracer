open Trace

type analysis_result = {
  dom_accesses: IntSet.t;
  inline_scripts: IntSet.t;
  async_scripts: IntSet.t;
  nondet: StringSet.t
}
let pp_analysis_result pp { dom_accesses; inline_scripts; async_scripts } =
  let open Fmt in
  let print = [("DOM accesses", dom_accesses);
               ("Inline scripts", inline_scripts);
               ("Async scripts", async_scripts)]
  in let print = BatList.filter (fun (_, s) -> not (IntSet.is_empty s)) print
  in if print = [] then
    string pp "(nothing)"
  else 
    list ~sep:(suffix sp (const string ";"))
      (box ~indent:2 (pair ~sep:(const string ": ")
                        string (IntSet.pp ~sep:sp)))
      pp print

module AnalysisStrategy = struct
  type data = analysis_result
  type edge = DependencyGraph.edge
  type vertex = DependencyGraph.vertex
  type g = DependencyGraph.t
  let direction = Graph.Fixpoint.Backward
  let join
        { dom_accesses = d1; nondet = n1;
          inline_scripts = i1; async_scripts = a1 }
        { dom_accesses = d2; nondet = n2;
          inline_scripts = i2; async_scripts = a2 } =
    { dom_accesses = IntSet.union d1 d2;
      inline_scripts = IntSet.union i1 i2;
      async_scripts = IntSet.union a1 a2;
      nondet = StringSet.union n1 n2
    }
  let equal 
        { dom_accesses = d1; nondet = n1;
          inline_scripts = i1; async_scripts = a1 }
        { dom_accesses = d2; nondet = n2;
          inline_scripts = i2; async_scripts = a2 } =
    IntSet.equal d1 d2 &&
      IntSet.equal i1 i2 &&
      IntSet.equal a1 a2 &&
      StringSet.equal n1 n2

  let analyze (_: edge) (r: analysis_result) = r
end

module DominationAnalysis =
  Graph.Fixpoint.Make(DependencyGraph)(AnalysisStrategy)
let calculate_domination has_nondeterminism has_dom_write cl depgraph =
  Logs.debug ~src:!Log.source (fun m -> m "Calculating domination facts");
  try
    let cond_singleton p v =
      if p then IntSet.singleton v else IntSet.empty
    in let maybe_singleton s v =
      cond_singleton (IntSet.mem v s) v
    in DominationAnalysis.analyze
         (fun v ->
            let open ClassifyTask in
              { dom_accesses = maybe_singleton has_dom_write v;
                inline_scripts =
                  cond_singleton (IntMap.find v cl = InlineScript) v;
                async_scripts =
                  cond_singleton (IntMap.find v cl = ExternalAsyncScript) v;
                nondet =
                  try IntMap.find v has_nondeterminism
                  with Not_found -> StringSet.empty 
              })
         depgraph
  with Not_found ->
    failwith "Not_found in calculate_domination"

