open ClassifyTask
open Trace
open PruneOrderGraph

type classifier =
  | Inline
  | Async
  | Defer
  | Inserted
  | DOMaccess
  | BlockedBySuccessor
  | Deferable
let classifier_string = function
  | Inline -> "inline script"
  | Async -> "already-async script"
  | Defer -> "already-defer script"
  | Inserted -> "run-time inserted script"
  | DOMaccess -> "script performing DOM access"
  | BlockedBySuccessor -> "script cannot be deferred due to blocked successor"
  | Deferable -> "deferable script"
let pp_classifier = Fmt.(using classifier_string string)

let classify_scripts order classification dom =
  let rec backpropagate todo dom =
    match todo with
      | v :: todo ->
          if not (DependencyGraph.mem_vertex order v) then
            backpropagate todo (IntSet.add v dom)
          else if IntSet.mem v dom then
            backpropagate todo dom
          else
            backpropagate (DependencyGraph.pred order v @ todo)
              (IntSet.add v dom)
      | [] -> dom
  in let propagated_dom = backpropagate (IntSet.to_list dom) dom
  in IntMap.filter_map
       (fun s c ->
          match c with
            | ToplevelScript
            | DelayedScript
            | SomeScript ->
                if IntSet.mem s propagated_dom then
                  Some DOMaccess
                else 
                  Some Deferable
            | _ -> None)
       classification

let dom_reference = function
  | RDOMNode _
  | RTree _
  | RDOMNodeAttribute _ -> true
  | _ -> false

let dom_access cmds =
  let rec check scope cmds = match scope, cmds with
    | None, Enter s :: cmds when is_javascript_scope s ->
        check (Some 0) cmds
    | Some i, Enter _ :: cmds ->
        check (Some (i+1)) cmds
    | Some 0, Exit :: cmds ->
        check None cmds
    | Some i, Exit ::  cmds ->
        check (Some (i-1)) cmds
    | Some _, Write (ref, _) :: cmds ->
        dom_reference ref || check scope cmds
    | _, _::cmds ->
        check scope cmds
    | _, [] -> false
  in check None cmds

let handle_log reasons remove filename =
  let base = Filename.chop_suffix (Filename.basename filename) ".log" in
    if !reasons then
      OrderGraph.log (open_out (base ^ ".reasons"));
  let { trace; pruned_order_graph; classification } =
    filename
      |> CleanLog.load
      |> Trace.parse_trace
      |> PruneOrderGraph.prune_order_graph (fun _ _ _ _ -> !remove)
  in let _ = remove := []
  in let dom_accesses =
    BatList.fold_left
      (fun dom { id; commands } ->
         if dom_access commands then IntSet.add id dom else dom)
      IntSet.empty trace.events
  in let script_classification =
    classify_scripts pruned_order_graph classification dom_accesses
  in let open Fmt in
    Format.printf "@[<v>%s: %d scripts@,@,%a@,@]"
      filename (IntMap.cardinal script_classification)
      (IntMap.pp_default ~esep:cut ~psep:(const string ": ") pp_classifier)
      script_classification

let () =
  let reasons = ref false
  and remove = ref [] in
  Arg.parse [
    ("-r", Arg.Set reasons, "record reasons");
    ("-p", Arg.Int (fun v -> remove := v :: !remove), "remove below this node");
    ("-G", Arg.Set OrderGraph.guid_heuristic, "GUID heuristic (HACK)");
  ] (handle_log reasons remove) ""
