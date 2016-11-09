open Trace

type classification =
  | InlineScript
  | ExternalSyncScript
  | ExternalAsyncScript
  | ExternalDeferScript
  | ExternalUnknownScript
  | ImmediateEventHandlerScript
  | ShortTimerEventHandlerScript
  | LongTimerEventHandlerScript
  | AnimationEventHandlerScript
  | ResourceLoadEventHandlerScript
  | WindowInteractiveScript
  | WindowCompleteScript
  | UIEventHandlerScript
  | OtherEventHandlerScript
  | UnclearScript
  | HTMLParsing
  | EventHandlerNoScript
  | NetworkWait
  | Other [@@deriving show]
let is_script = function
  | InlineScript
  | ExternalSyncScript
  | ExternalAsyncScript
  | ExternalDeferScript
  | ExternalUnknownScript
  | ImmediateEventHandlerScript
  | ShortTimerEventHandlerScript
  | LongTimerEventHandlerScript
  | AnimationEventHandlerScript
  | ResourceLoadEventHandlerScript
  | WindowInteractiveScript
  | WindowCompleteScript
  | UIEventHandlerScript
  | OtherEventHandlerScript
  | UnclearScript -> true
  | _ -> false
let is_toplevel_script = function
  | InlineScript
  | ExternalSyncScript
  | ExternalAsyncScript
  | ExternalDeferScript
  | ExternalUnknownScript -> true
  | _ -> false
let is_event_handler = function
  | ImmediateEventHandlerScript
  | ShortTimerEventHandlerScript
  | LongTimerEventHandlerScript
  | AnimationEventHandlerScript
  | ResourceLoadEventHandlerScript
  | WindowInteractiveScript
  | WindowCompleteScript
  | UIEventHandlerScript
  | OtherEventHandlerScript
  | UnclearScript -> true
  | _ -> false

(* Patterns, from most to least clear:
 * scopes HTML, JS [GlobalCode]: InlineScript
 * scope 'script runner timer': ExternalAsyncScript
 * read of some CachedResouce, JS [GlobalCode]:
 *   ExternalSyncScript or ExternalDeferScript
 *   (depends on global state: before or after document.readyStateChange)
 * scope HTML, no JS scope: HTML parsing
 * ----
 * Read -> animation request handler function, reads*, post id:
 *   id is AnimationEventHandler or EventHandlerNoScript
 *   (depending on trace of Event id)
 * Enter DOMTimer, post id:
 *   id is ShrtoTimerEventHandlerScript, LongTimerEventHandlerScript
 *   or EventHandlerNoScript
 *   (depending on trace of Event id and duration given in the
 *   dependency edge; cut-off configurable, default 10ms).
 * ----
 * Event start with event handler scope
 *   One of ResourceLoadEventHandlerScript,
 *   WindowLoadEventHandlerScript,
 *   ReadyStateChangeEventHandlerScript,
 *   UIEventHandlerScript or
 *   EventHandlerNoScript.
 * ----
 * Anything not yet classified, but containing JS code:
 * UnclearScript
 * ----
 * Event from source network, with only posts and access
 * to CachedResource: NetworkWait
 * ----
 * Other *)
type state = {
  outstanding_animation_frames: IntSet.t;
  animation_frame_request_functions: int list;
  classification: classification IntMap.t [@opaque];
  parsing_may_be_suspended: bool
} [@@deriving show]

let is_toplevel_script_scope  = function
  | JSCode { jstype = GlobalCode }
  | JSONDeclareGlobal
  | JSONDeclareGlobalvar
  | JSDeclareFunction
  | JSDeclareGlobalvar -> true
  | _ -> false

type event_kind =
    ImmediateTimerEvent | ShortTimerEvent | LongTimerEvent
    | UIEvent | AnimationEvent | DocInteractiveEvent
    | DocLoadedEvent | ResourceEvent
    [@@deriving show]

type inner_state = {
  saw_html: bool;
  potential_animation_request: bool;
  event_kind: event_kind option;
  saw_cached_resource: bool;
  network_resource: bool;
  saw_script_runner_timer: bool;
  type_known: classification option;
} [@@deriving show]

let update_saw_html inner_state = function
  | Enter (Parse _) -> { inner_state with saw_html = true; saw_cached_resource = false }
  | _ -> inner_state

let update_animation_requests (outer_state, inner_state) = function
  | Read (RHeap { id } , _)
      when List.mem id outer_state.animation_frame_request_functions ->
      if inner_state.type_known = None then
        Format.eprintf
          "Warning: Saw potential animation request outside JS code@.";
      (outer_state,
       { inner_state with potential_animation_request = true })
  | Read (RHeap { objtype = "Window";
                  prop = "webkitRequestAnimationFrame" },
          VObject { id })
  | Read (RHeap { objtype = "Window";
                  prop = "requestAnimationFrame" },
          VObject { id }) ->
      ({ outer_state with animation_frame_request_functions =
           id :: outer_state.animation_frame_request_functions },
       inner_state)
  | Read _ -> (outer_state, inner_state)
  | Post id when id <> -1 && inner_state.potential_animation_request ->
      ({ outer_state with outstanding_animation_frames =
           IntSet.add id outer_state.outstanding_animation_frames },
       { inner_state with potential_animation_request = false })
  | _ -> (outer_state,
          { inner_state with potential_animation_request = false })

let find_event_type = function
  | "DOMContentLoaded" -> DocInteractiveEvent
  | "load" -> DocLoadedEvent
  | "afterprint" | "beforeprint" | "beforeunload" | "blur"
  | "cancel" | "change" | "click" | "contextmenu"
  | "copy" | "cut" | "focus" | "hashchange"
  | "input" | "invalid" | "languagechange"
  | "message" | "offline" | "online" | "pagehide"
  | "pageshow" | "paste" | "popstate" | "progress" | "reset"
  | "select" | "show" | "storage" | "submit" | "toggle"
  | "unload" | "beforecopy" | "beforecut" | "DOMFocusIn"
  | "DOMNodeInserted" | "focusin" | "keydown"
  | "lazybeforeunveil" | "mouseover" | "mouseout"
  | "mousemove" | "osd_load" -> UIEvent
  | "connect" | "error"  | "loadend"
  | "loadstart" | "open" | "rejectionhandled" | "unhandledrejection"
  | "CookieEventAPI" | "readystatechange" | "Resolved"
  | "wtBeaconSent" -> ResourceEvent
  | s -> Format.eprintf "Unknown event type %s, guessing UI@." s;
         UIEvent

let update_event_kind inner_state cmd =
  if inner_state.type_known = None && inner_state.event_kind = None then
    match cmd with
      | Enter (EventFire FireAnchor) ->
          { inner_state with event_kind = Some UIEvent }
      | Enter (EventFire FireDefault) ->
          { inner_state with event_kind = Some UIEvent }
      | Enter (EventFire (FireTarget s | FireCapture s | FireBubble s)) ->
          { inner_state with event_kind = Some (find_event_type s) }
      | Enter (Event s) ->
          { inner_state with event_kind = Some (find_event_type s) }
      | Enter (EventHandler _) ->
          { inner_state with event_kind = Some UIEvent }
      | _ -> inner_state
  else inner_state

let update_saw_cached_resource inner_state = function
  | Read (RCachedResource _, _)
  | Write (RCachedResource _, _)
  | Enter CachedResource -> { inner_state with saw_cached_resource = true }
  | _ -> inner_state
let update_network_resource inner_state = function
  | Read (RCachedResource _, _)
  | Read (RScriptRunner _, _)
  | Write (RCachedResource _, _)
  | Write (RScriptRunner _, _)
  | Post _ -> inner_state
  | _ -> { inner_state with network_resource = false }
let update_saw_script_runner_timer inner_state = function
  | Enter TimerScript -> { inner_state with saw_script_runner_timer = true }
  | _ -> inner_state

let from_event_type = function
  | ImmediateTimerEvent -> ImmediateEventHandlerScript
  | ShortTimerEvent -> ShortTimerEventHandlerScript
  | LongTimerEvent -> LongTimerEventHandlerScript
  | AnimationEvent -> AnimationEventHandlerScript
  | ResourceEvent -> ResourceLoadEventHandlerScript
  | DocInteractiveEvent -> WindowInteractiveScript
  | DocLoadedEvent -> WindowCompleteScript
  | UIEvent -> UIEventHandlerScript

let update_type_known outer_state inner_state cmd =
  if inner_state.type_known <> None then inner_state
  else match cmd with
    | Enter JSONDeclareGlobal
    | Enter JSONDeclareGlobalvar
    | Enter JSDeclareFunction
    | Enter JSDeclareGlobalvar
    | Enter (JSCode { jstype = GlobalCode }) ->
        if inner_state.saw_html && not inner_state.saw_cached_resource then
          { inner_state with type_known = Some InlineScript }
        else if inner_state.saw_html then
          (* Cached sync script *)
          { inner_state with type_known = Some ExternalSyncScript }
        else if inner_state.saw_script_runner_timer then
          { inner_state with type_known = Some ExternalAsyncScript }
        else if inner_state.saw_cached_resource &&
                outer_state.parsing_may_be_suspended &&
                inner_state.network_resource
        then
          { inner_state with type_known = Some ExternalSyncScript }
        else if inner_state.saw_cached_resource &&
                inner_state.network_resource then
          { inner_state with type_known = Some ExternalDeferScript }
        else begin match inner_state.event_kind with
            Some e ->
              { inner_state with type_known = Some (from_event_type e) }
          | None ->
              Format.eprintf "@[<v2>Can't classify script type (top-level)@,inner state: @[<hov>%a@]@,outer state: @[<hov>%a@]@,@]@."
                pp_inner_state inner_state pp_state outer_state;
              { inner_state with type_known = Some UnclearScript }
        end
    | Enter JSExec _
    | Enter JSCall _
    | Enter JSCode _ ->
        begin match inner_state.event_kind with
            Some e ->
              { inner_state with type_known = Some (from_event_type e) }
          | None ->
              Format.eprintf "@[<v2>Can't classify script type (reference)@,inner state: @[<hov>%a@]@,outer state: @[<hov>%a@]@,@]@."
                pp_inner_state inner_state pp_state outer_state;
              { inner_state with type_known = Some UnclearScript }
        end
    | _ -> inner_state

let finalize_type { saw_html; event_kind; network_resource; type_known } =
  match type_known, event_kind with
    | Some ty, _ -> ty
    | None, Some ev -> EventHandlerNoScript
    | None, None ->
        if saw_html then
          HTMLParsing
        else if not network_resource then
          Other
        else
          NetworkWait

let delay_cutoff = 10

let update_outstanding_animation_frames inner_state outer_state cmd =
  if inner_state.potential_animation_request then
    match cmd with
      | Post id when id <> -1 ->
          { outer_state with outstanding_animation_frames =
              IntSet.add id outer_state.outstanding_animation_frames }
      | _ -> outer_state
  else outer_state

let update_animation_frame_request_functions outer_state = function
  | Read (RHeap { objtype = "Window";
                  prop = "webkitRequestAnimationFrame" },
          VObject { id })
  | Read (RHeap { objtype = "Window";
                  prop = "requestAnimationFrame" },
          VObject { id }) ->
      { outer_state with animation_frame_request_functions =
          id :: outer_state.animation_frame_request_functions }
  | _ -> outer_state

let update_parsing_may_be_suspended inner_state outer_state =
  { outer_state with parsing_may_be_suspended =
      (inner_state.saw_html ||
      (inner_state.network_resource &&
       outer_state.parsing_may_be_suspended)) }

let update_classification id inner_state outer_state =
  { outer_state with classification =
      IntMap.add id (finalize_type inner_state)
        outer_state.classification }

let update_command deps id (outer_state, inner_state) command =
  let inner_state = update_type_known outer_state inner_state command
  in let (outer_state, inner_state) =
    update_animation_requests (outer_state, inner_state) command
  in let inner_state = update_saw_html inner_state command
  in let inner_state = update_saw_cached_resource inner_state command
  in let inner_state = update_network_resource inner_state command
  in let inner_state = update_saw_script_runner_timer inner_state command
  in let inner_state = update_event_kind inner_state command
  in (outer_state, inner_state)
let initial_inner_state = {
  saw_html = false;
  potential_animation_request = false;
  event_kind = None;
  saw_cached_resource = false;
  network_resource = true;
  saw_script_runner_timer = false;
  type_known = None;
}
let initial_event_kind deps id { outstanding_animation_frames } =
  let delay =
    if DependencyGraph.mem_vertex deps id then
      DependencyGraph.fold_pred_e
        (fun (_, lbl, _) delay ->
           match lbl with
             | Some delay' -> max delay delay'
             | None -> delay)
        deps id (-1)
    else -1
  in Format.eprintf "Delay for %d: %d@." id delay;
    if delay = 0 then
    Some ImmediateTimerEvent
  else if 0 < delay && delay < delay_cutoff then
    Some ShortTimerEvent
  else if delay_cutoff <= delay then
    Some LongTimerEvent
  else if IntSet.mem id outstanding_animation_frames then
    Some AnimationEvent
  else
    None

let update_event deps outer_state { id; evtype; commands } =
  let inner_state = { initial_inner_state with
                          event_kind = initial_event_kind deps id outer_state }
  in let (outer_state, inner_state) =
    BatList.fold_left (update_command deps id)
      (outer_state, inner_state) commands
  in let outer_state = update_classification id inner_state outer_state
  in let outer_state = update_parsing_may_be_suspended inner_state outer_state
  in outer_state
let initial_outer_state = {
  outstanding_animation_frames = IntSet.empty;
  animation_frame_request_functions = [];
  classification = IntMap.empty;
  parsing_may_be_suspended = false
}
let collect { events; deps } =
  BatList.fold_left (update_event deps) initial_outer_state events

let has_proper_parse_html { commands } =
  BatList.exists (function
                    | Enter (Parse source) -> source <> ""
                    | _ -> false)
    commands

let remove_junk { events; deps } =
  let rec loop deps = function
    | (event :: _) as events when has_proper_parse_html event ->
        { events; deps }
    | { id } :: events ->
        loop (DependencyGraph.remove_vertex deps id) events
    | [] -> { events = []; deps = DependencyGraph.empty }
  in loop deps events

let classify trace =
  let trace = remove_junk trace
  in (trace, (collect trace).classification)

