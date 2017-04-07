(* Mark DOM writes and non-determinism *)
type markings = {
  has_dom_write: IntSet.t;
  has_nondeterminism: StringSet.t IntMap.t;
}

let rec fold_js_commands_impl f level commands acc =
  let open Trace in
  match level, commands with
    | _, [] -> acc
    | Some l, (Enter _ as command) :: commands ->
        fold_js_commands_impl f (Some (l+1)) commands (f acc command)
    | None, Enter scope :: commands when is_javascript_scope scope ->
        fold_js_commands_impl f (Some 0) commands (f acc (Enter scope))
    | Some 0, Exit :: commands ->
        fold_js_commands_impl f None commands (f acc Exit)
    | Some l, Exit :: commands ->
        fold_js_commands_impl f (Some (l-1)) commands (f acc Exit)
    | Some _, command :: commands ->
        fold_js_commands_impl f level commands (f acc command)
    | None, _ :: commands ->
        fold_js_commands_impl f level commands acc

let fold_js_command f { Trace.commands } acc =
  fold_js_commands_impl f None commands acc

let nondet_heap = [
  ("Math", "random");
  ("Function", "now") (* Date resolves to a Function object *);
  ("Window", "getComputedStlye");
  ("Window", "getOverrideStlye");
  ("DOMImplementation", "hasFeature");
  ("DOMImplementation", "getFeature");
  ("Window", "name;");
  ("Window", "innerHeight;");
  ("Window", "innerWidth;");
  ("Window", "scrollX;");
  ("Window", "scrollY;");
  ("Window", "pageXOffset;");
  ("Window", "pageYOffset;");
  ("Window", "screenX;");
  ("Window", "screenY;");
  ("Window", "outerHeight;");
  ("Window", "outerWidth;");
  ("Window", "devicePixelRatio;");
  ("Window", "getSelection;");
  ("Window", "orientation;")
]

let nondet_string = function
  | Trace.RHeap { objtype; prop } ->
      objtype ^ "." ^ prop
  | _ -> failwith "Unknown nondeterminism source"

let is_nondet_ref = function
  | Trace.RHeap { objtype; prop } ->
      List.mem (objtype, prop) nondet_heap
  | _ -> false

let calculate_markings_for_event { has_dom_write; has_nondeterminism } =
  let open Trace in fun ({ id } as event) ->
  let (dom, nondet) =
    fold_js_command (fun (dom, nondet) -> function
                       (*| Read (ref, _) when is_nondet_ref ref ->
                           (dom, StringSet.add (nondet_string ref) nondet)*)
                       | Enter (Env s | Nondet s) ->
                           (dom, StringSet.add s nondet)
                       | Write (RDOMNode _, _)
                       | Write (RDOMNodeAttribute _, _)
                       | Write (RMemCell _, _)
                       | Write (RTree _, _) ->
                           (true, nondet)
                       | _ -> (dom, nondet))
      event (false, StringSet.empty)
  in { has_dom_write =
         if dom then IntSet.add id has_dom_write else has_dom_write;
       has_nondeterminism =
         if StringSet.is_empty nondet then
           has_nondeterminism
         else
           IntMap.add id nondet has_nondeterminism }

let calculate_markings { Trace.events } =
  Log.debug (fun m -> m "Calculating markings");
  BatList.fold_left calculate_markings_for_event
    { has_nondeterminism = IntMap.empty;
      has_dom_write = IntSet.empty }
    events
