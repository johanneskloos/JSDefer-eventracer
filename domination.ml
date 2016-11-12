open Trace
open ReducedOrderGraph

type analysis_result = {
  dom_accesses: IntSet.t;
  inline_scripts: IntSet.t;
  async_scripts: IntSet.t
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
        { dom_accesses = d1;
          inline_scripts = i1; async_scripts = a1 }
        { dom_accesses = d2;
          inline_scripts = i2; async_scripts = a2 } =
    { dom_accesses = IntSet.union d1 d2;
      inline_scripts = IntSet.union i1 i2;
      async_scripts = IntSet.union a1 a2 }
  let equal 
        { dom_accesses = d1;
          inline_scripts = i1; async_scripts = a1 }
        { dom_accesses = d2;
          inline_scripts = i2; async_scripts = a2 } =
    IntSet.equal d1 d2 &&
      IntSet.equal i1 i2 &&
      IntSet.equal a1 a2
  let analyze (_: edge) (r: analysis_result) = r
end

module DominationAnalysis =
  Graph.Fixpoint.Make(DependencyGraph)(AnalysisStrategy)
let calculate_domination
      { has_nondeterminism; has_dom_write } cl
      depgraph =
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
             cond_singleton (IntMap.find v cl = ExternalAsyncScript) v })
         depgraph
  with Not_found ->
    failwith "Not_found in calculate_domination"

type verdict =
    (* Deferable cases *)
    Deferable
  | Deferred
    (* Non-deferable cases *)
  | HasDOMAccess
  | IsInlineScript
  | IsAsyncScript
  | DominatedByDOMAccess
  | DominatedByInlineScript
  | DominatedByAsyncScript
let verdict_to_string = function
    (* Deferable cases *)
    Deferable -> "deferable"
  | Deferred -> "already deferred"
    (* Non-deferable cases *)
  | HasDOMAccess -> "performs DOM write"
  | IsInlineScript -> "is an inline script"
  | IsAsyncScript -> "is an async script"
  | DominatedByDOMAccess -> "dominated by a DOM-writing script"
  | DominatedByInlineScript -> "dominated by an inline script"
  | DominatedByAsyncScript -> "dominated by an async script"
let pp_verdict = Fmt.using verdict_to_string Fmt.string

type result = {
  verdict: verdict;
  nondet: bool;
  data: analysis_result
}
let pp_result pp { verdict; nondet; data } =
  let open Fmt in
  match verdict with
    | DominatedByDOMAccess
    | DominatedByAsyncScript
    | DominatedByInlineScript ->
        pf pp "%a%s: %a"
          pp_verdict verdict
          (if nondet then " (non-deterministic)" else "")
          pp_analysis_result data
    | _ ->
        pf pp "%a%s"
          pp_verdict verdict
          (if nondet then " (non-deterministic)" else "")

let deferability_analysis cl { has_nondeterminism } dom =
  let open ClassifyTask in
  IntMap.filter_map
    (fun v vc ->
       try
         let ve = match vc with
           | ExternalSyncScript ->
               let { dom_accesses; inline_scripts; async_scripts } = dom v  in
                 if IntSet.mem v dom_accesses then HasDOMAccess
                 else if not (IntSet.is_empty dom_accesses) then DominatedByDOMAccess
                 else if not (IntSet.is_empty inline_scripts) then DominatedByInlineScript
                 else if not (IntSet.is_empty async_scripts) then DominatedByAsyncScript
                 else Deferable
           | ExternalAsyncScript -> IsAsyncScript
           | ExternalDeferScript -> Deferred
           | InlineScript -> IsInlineScript
           | UnclearScript -> failwith "Unclear script type"
           | _ -> raise Exit
         in Some { verdict = ve; nondet = IntMap.mem v has_nondeterminism;
                   data = dom v }
       with Exit -> None | Not_found -> None)
    cl

let calculate_domination trace =
  let (trace, cl, data, data', dcl_pre, depgraph) =
    calculate trace
  in let dom = calculate_domination data' cl depgraph
  in let def = deferability_analysis cl data' dom
  in (trace, cl, data, data', dcl_pre, depgraph, dom, def)

