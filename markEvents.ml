open Trace

(* Mark DOM writes and non-determinism *)
type markings = {
  has_dom_write: IntSet.t;
  has_nondeterminism: IntSet.t;
}

let rec fold_js_commands_impl f level commands acc =
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

let fold_js_command f { commands } acc =
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

let is_nondet_ref = function
  | RHeap { objtype; prop } ->
      List.mem (objtype, prop) nondet_heap
  | _ -> false

let calculate_markings_for_event { has_dom_write; has_nondeterminism }
      ({ id } as event) =
  let (dom, nondet) =
    fold_js_command (fun (dom, nondet) -> function
                       | Read (ref, _) when is_nondet_ref ref ->
                           (dom, true)
                       | Write (RDOMNode _, _)
                       | Write (RDOMNodeAttribute _, _)
                       | Write (RMemCell _, _)
                       | Write (RTree _, _) ->
                           (true, nondet)
                       | _ -> (dom, nondet))
      event (false, false)
  in { has_dom_write =
         if dom then IntSet.add id has_dom_write else has_dom_write;
       has_nondeterminism =
         if nondet then IntSet.add id has_nondeterminism else
           has_nondeterminism }

let calculate_markings { events } =
  BatList.fold_left calculate_markings_for_event
    { has_nondeterminism = IntSet.empty;
      has_dom_write = IntSet.empty }
    events
