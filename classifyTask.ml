open Trace

type classification =
  | ToplevelScript
  | EventHandlerScript
  | DelayedScript
  | EventHandler
  | SomeScript
  | HTMLStep
  | Other [@@deriving show]

let is_parsing_flotsam = function
  | Write (RDOMNode _, _)
  | Write (RTree _, _)
  | Write (RCachedResource _, _)
  | Post _ -> true
  | _ -> false

let is_toplevel_script = function
  | Enter (Parse _) :: rest ->
      begin match BatList.drop_while is_parsing_flotsam rest
      with
        | Enter (JSCode { jstype = GlobalCode }) :: _
        | Enter JSONDeclareGlobal :: _
        | Enter JSONDeclareGlobalvar :: _
        | Enter JSDeclareFunction :: _
        | Enter JSDeclareGlobalvar :: _ -> true
        | _ -> false
      end
  | _ -> false

let is_script =
  BatList.exists (function Enter c -> is_javascript_scope c | _ -> false)

let rec is_event_handler_script = function
  | Post _ :: rest -> is_event_handler_script rest
  | Enter (ResourceOperation _) :: rest -> is_event_handler_script rest
  | Enter (Event _) :: rest -> is_script rest
  | Read _ :: rest -> is_event_handler_script rest
  | Enter (EventFire _) :: rest -> is_script rest
  | Enter (EventQueue _) :: rest -> is_script rest
  | Enter (EventHandler _) :: rest -> is_script rest
  | _ -> false

let is_delayed_flotsam = function
  | Read (RCachedResource _, _)
  | Write (RCachedResource _, _)
  | Post _ -> true
  | _ -> false

let is_script =
  BatList.exists (function Enter c -> is_javascript_scope c | _ -> false)

let is_toplevel_script_entry = function
  | Enter (JSCode { jstype = GlobalCode }) :: _
  | Enter JSONDeclareGlobal :: _
  | Enter JSONDeclareGlobalvar :: _
  | Enter JSDeclareFunction :: _
  | Enter JSDeclareGlobalvar :: _ -> true
  | _ -> false

let rec is_delayed_script = function
  | Enter TimerScript :: _ -> true
  | Post _ :: rest -> is_delayed_script rest (* FIXME: Why do we see this? *)
  | Read (RCachedResource _, _) :: rest
  | Write (RCachedResource _, _) :: rest ->
      is_toplevel_script_entry (BatList.drop_while is_delayed_flotsam rest)
  | _ -> false

let is_event_handler c = false
let is_parse_step = function
  | Enter (Parse _) :: _ -> true
  | _ -> false

let classify_event dom_timer_handlers animation_handlers { id; commands } =
  (id,
   if is_toplevel_script commands then ToplevelScript
   else if IntSet.mem id dom_timer_handlers && is_script commands then EventHandlerScript
   else if IntSet.mem id animation_handlers && is_script commands then EventHandlerScript
   else if is_event_handler_script commands then EventHandlerScript
   else if is_delayed_script commands then DelayedScript
   else if is_event_handler commands then EventHandler
   else if is_script commands then SomeScript
   else if is_parse_step commands then HTMLStep
   else Other)

let find_dom_timer_handlers { events; deps } =
  let rec find src dth = function
    | Enter TimerDOM ::
      Post tgt ::
      Write (RTimer _, VDOMTimer _) ::
      Exit :: rest ->
        if tgt = -1 then find src dth rest
        else find src (IntSet.add tgt dth) rest
    | _ :: rest -> find src dth rest
    | [] -> dth
  in BatList.fold_left
       (fun dth { id = src; commands } -> 
          (* Special handling for interval timers *)
          if IntSet.mem src dth then match commands with
            | Post id' :: commands ->
                (* This is an interval timer *)
                find src (IntSet.add id' dth) commands
            | _ -> find src dth commands
              else find src dth commands)
       IntSet.empty events

let find_animation_requests { events } =
  let find_req req { commands } =
    List.fold_left (fun req ev -> match ev with
                      | Read (RHeap { objtype = "Window";
                                      prop = "webkitRequestAnimationFrame" },
                              (VObject _ as f)) ->
                          f :: req
                      | Read (RHeap { objtype = "Window";
                                      prop = "requestAnimationFrame" },
                              (VObject _ as f)) ->
                          f :: req
                      | _ -> req)
      req commands
  in List.fold_left find_req [] events
      
let find_animation_handlers reqs { events } =
  (* Heuristic. Let's see if this is enough. *)
  let rec find_ani ani = function
    | Read (_, req) :: Post id :: rest
    | Read (_, req) :: Read _ :: Post id :: rest
    | Read (_, req) :: Read _ :: Read _ :: Post id :: rest
    | Read (_, req) :: Read _ :: Read _ :: Read _ :: Post id :: rest
      when List.mem req reqs ->
        find_ani (IntSet.add id ani) rest
    | _ :: rest -> find_ani ani rest
    | [] -> ani
  in List.fold_left (fun ani { commands } -> find_ani ani commands)
       IntSet.empty events

let classify ({ events } as trace) =
  let dom_timer_handlers = find_dom_timer_handlers trace
  and animation_requests = find_animation_requests trace
  in let animation_handlers = find_animation_handlers animation_requests trace
  in (trace, List.fold_left
               (fun classification event ->
                  let (id, class_) =
                    (classify_event dom_timer_handlers
                       animation_handlers event)
                  in IntMap.add id class_ classification)
               IntMap.empty events)


