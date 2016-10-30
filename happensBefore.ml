open Trace

let enrich_hb { deps; events } =
  let deps = BatList.fold_left
               (fun deps { id = src; commands } ->
                  BatList.fold_left
                    (fun deps -> function
                       | Post tgt -> DependencyGraph.add_edge deps src tgt
                       | _ -> deps)
                    deps commands)
               deps events
  in { deps; events }

