open Trace
open ReducedOrderGraph

type mode = DOMaccess | Dominated | NotDominated

module DOMFixpoint = struct
  type data = mode
  type edge = DependencyGraph.edge
  type vertex = DependencyGraph.vertex
  type g = DependencyGraph.t
  let direction = Graph.Fixpoint.Backward
  let join m1 m2 = match m1, m2 with
    | NotDominated, NotDominated -> NotDominated
    | DOMaccess, _ -> DOMaccess
    | _, DOMaccess -> DOMaccess
    | _, _ -> Dominated
  let equal: mode -> mode -> bool = (=)
  let analyze (_: edge) = function
    | NotDominated -> NotDominated
    | _ -> Dominated
end
module DOMAnalysis = Graph.Fixpoint.Make(DependencyGraph)(DOMFixpoint)

let calculate_dominated_set { has_dom_write } depgraph =
  try
    DOMAnalysis.analyze
      (fun v -> if IntSet.mem v has_dom_write then DOMaccess else NotDominated)
      depgraph
  with Not_found -> failwith "Not_found in calculate_dominated_set"

type verdict = Deferable | DeferableNondet | IsDominated | HasDOMAccess | NotSyncScript

let deferability_analysis cl { has_nondeterminism; spec } dom =
  IntMap.mapi (fun v _ ->
                 match IntMap.find v cl with
                   | ClassifyTask.ExternalSyncScript ->
                       begin match dom v with
                           NotDominated ->
                             if IntSet.mem v has_nondeterminism then
                               DeferableNondet
                             else
                               Deferable
                         | Dominated -> IsDominated
                         | DOMaccess -> HasDOMAccess
                         | exception Not_found -> Format.eprintf "%d not marked in domination graph" v;
                                                  Deferable
                       end
                   | _ -> NotSyncScript
                   | exception Not_found -> failwith "Not_found in cl in deferability_analysis")
    spec

let calculate_domination trace =
  let (trace, cl, data, data', dcl_pre, depgraph) =
    calculate trace
  in let dom = calculate_dominated_set data' depgraph
  in let def = deferability_analysis cl data' dom
  in (trace, cl, data, data', dcl_pre, depgraph, dom, def)
