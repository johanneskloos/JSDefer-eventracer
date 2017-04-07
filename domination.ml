module DependencyGraph = Trace.DependencyGraph

type analysis_result = {
  dom_accesses: IntSet.t;
  inline_scripts: IntSet.t;
  async_scripts: IntSet.t;
  nondet: StringSet.t;
  races_with : Races.RaceSet.t
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
        { dom_accesses = d1; nondet = n1; races_with = r1;
          inline_scripts = i1; async_scripts = a1 }
        { dom_accesses = d2; nondet = n2; races_with = r2;
          inline_scripts = i2; async_scripts = a2 } =
    { dom_accesses = IntSet.union d1 d2;
      inline_scripts = IntSet.union i1 i2;
      async_scripts = IntSet.union a1 a2;
      nondet = StringSet.union n1 n2;
      races_with = Races.RaceSet.union r1 r2
    }
  let equal 
        { dom_accesses = d1; nondet = n1; races_with = r1;
          inline_scripts = i1; async_scripts = a1 }
        { dom_accesses = d2; nondet = n2; races_with = r2;
          inline_scripts = i2; async_scripts = a2 } =
    IntSet.equal d1 d2 &&
      IntSet.equal i1 i2 &&
      IntSet.equal a1 a2 &&
      StringSet.equal n1 n2 &&
      Races.RaceSet.equal r1 r2

  let analyze (_: edge) (r: analysis_result) = r
end

module DominationAnalysis =
  Graph.Fixpoint.Make(DependencyGraph)(AnalysisStrategy)
module Dfs = Graph.Traverse.Dfs(DependencyGraph)

let check_subset v1 v2 result =
  let { dom_accesses = d1; inline_scripts = i1;
        async_scripts = a1; nondet = n1} = result v1
  and { dom_accesses = d2; inline_scripts = i2;
        async_scripts = a2; nondet = n2} = result v2
  in let isp pp = IntSet.pp pp
  and ssp pp = StringSet.pp pp in
    if not (IntSet.subset d2 d1) then
      Log.err (fun m -> m "Found bad propagation: %d -> %d, dom: {%a} ̸̸⊆ {%a}"
                          v2 v1 isp d2 isp d1);
    if not (IntSet.subset i2 i1) then
      Log.err (fun m -> m "Found bad propagation: %d -> %d, inl: {%a} ̸̸⊆ {%a}"
                          v2 v1 isp i2 isp i1);
    if not (IntSet.subset a2 a1) then
      Log.err (fun m -> m "Found bad propagation: %d -> %d, asy: {%a} ̸̸⊆ {%a}"
                          v2 v1 isp a2 isp a1);
    if not (StringSet.subset n2 n1) then
      Log.err (fun m -> m "Found bad propagation: %d -> %d, det: {%a} ̸̸⊆ {%a}"
                          v2 v1 ssp n2 ssp n1)

let sanity_check nd dw cl dep result =
  DependencyGraph.iter_vertex
    (fun v ->
       Dfs.prefix_component (fun v' ->
         check_subset v v' result)
         dep v;
       let { dom_accesses; inline_scripts; async_scripts;
             nondet } = result v
       in
         if IntMap.mem v nd && StringSet.is_empty nondet then
           Log.err (fun m -> m "Found bad nondet set: %d" v);
         if IntSet.mem v dw && not (IntSet.mem v dom_accesses) then
           Log.err (fun m -> m "Found bad dom_acc set: %d" v);
         match IntMap.find v cl with
           | ClassifyTask.InlineScript ->
               if not (IntSet.mem v inline_scripts) then
                 Log.err (fun m -> m "Found missing inline script: %d" v)
           | ClassifyTask.ExternalAsyncScript ->
               if not (IntSet.mem v async_scripts) then
                 Log.err (fun m -> m "Found missing async script: %d" v)
           | _ -> ())
    dep

let reflexive_closure g =
  DependencyGraph.fold_vertex (fun v g -> DependencyGraph.add_edge g v v)
    g g

let calculate_domination has_nondeterminism has_dom_write races cl depgraph =
  Log.debug (fun m -> m "Calculating domination facts");
  try
    let cond_singleton p v =
      if p then IntSet.singleton v else IntSet.empty
    in let maybe_singleton s v =
      cond_singleton (IntSet.mem v s) v
    in let result = DominationAnalysis.analyze
                      (fun v ->
                         let open ClassifyTask in
                           { races_with =
                           Races.RaceSet.filter
                             (fun { Races.script_ev } -> script_ev = v)
                             races;
                             dom_accesses = maybe_singleton has_dom_write v;
                             inline_scripts =
                               cond_singleton (IntMap.find v cl = InlineScript) v;
                             async_scripts =
                               cond_singleton (IntMap.find v cl = ExternalAsyncScript) v;
                             nondet =
                               try IntMap.find v has_nondeterminism
                               with Not_found -> StringSet.empty 
                           })
                      (reflexive_closure depgraph)
    in sanity_check has_nondeterminism has_dom_write cl depgraph result;
       result
  with Not_found ->
    failwith "Not_found in calculate_domination"

