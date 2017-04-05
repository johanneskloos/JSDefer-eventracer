type pointer = int64
let compare_pointer = BatInt64.compare

type value =
  | Vnull
  | Vundefined
  | Vbool of bool
  | Vint of int32
  | Vnullstring
  | Vstring of string
  | VDOMNode of pointer
  | VObject of { objtype: string; id: int }
  | VEvent of pointer
  | VDOMTimer of pointer
  | Vunknown [@@deriving ord]

let pp_value pp = let open Fmt in function
    | Vnull -> string pp "NULL"
    | Vundefined -> string pp "undefined"
    | Vbool b -> bool pp b
    | Vint i -> int32 pp i
    | Vnullstring -> string pp "null"
    | VDOMNode p -> pf pp "DOMNode[0x%Lx]" p
    | VDOMTimer p -> pf pp "DOMTimer[0x%Lx]" p
    | VObject { objtype; id } -> pf pp "%s[%d]" objtype id
    | VEvent p -> pf pp "Event[0x%Lx]" p
    | Vunknown -> string pp "(unknown value)"
    | Vstring s -> pf pp "\"%s\"" s
let show_value = Fmt.to_to_string pp_value

module Value = struct
  type t = value
  let compare = compare_value
end
module ValueSet = struct
  include BatSet.Make(Value)
  let pp ?sep = Fmt.iter ?sep iter pp_value
end
module ValueMap = struct
  include BatMap.Make(Value)
  let pp ?sep = Fmt.iter_bindings ?sep iter
  let pp_default ?esep ?psep fmt =
    pp ?sep:esep (Fmt.pair ?sep:psep pp_value fmt)
end

type reference =
  | RHeap of { objtype: string; id: int; prop: string }
  | RDOMNode of pointer
  | RScriptRunner of { runner: pointer; element: pointer }
  | REventHandler of pointer
  | RTree of { scope: pointer; id: string }
  | RDOMNodeAttribute of { node: pointer; attribute: string }
  | RCachedResource of { resource: pointer; client: pointer }
  | RTimer of int
  | RNodeEvent of { nodetype: string; node: pointer; handler: string }
  | RArrayCell of { id: int; index: int }
  | RArrayLength of int
  | RMemCell of { base: pointer; prop: string }
  | Runknown [@@deriving ord]
module Reference = struct
  type t = reference
  let compare = compare_reference
end

let pp_reference pp = let open Fmt in function
    | RHeap { objtype; id; prop } -> pf pp "%s[%d].%s" objtype id prop
    | RDOMNode p -> pf pp "NodeTree:0x%Lx" p
    | RScriptRunner { runner; element } -> pf pp "ScriptRunner-%Lx-%Lx" runner element
    | REventHandler p -> pf pp "Event[0x%Lx]" p
    | RTree { scope; id } -> pf pp "Tree[0x%Lx]:%s" scope id
    | RDOMNodeAttribute { node; attribute } -> pf pp "DOMNode[%Lx].%s" node attribute
    | RCachedResource { resource; client } -> pf pp "CachedResource-%Lx-%Lx" resource client
    | RTimer id -> pf pp "Timer:%d" id
    | RNodeEvent { nodetype; node; handler } -> pf pp "%s[%Lx].%s" nodetype node handler
    | RArrayCell { id; index } -> pf pp "Array[%d]$[%d]" id index
    | RArrayLength id -> pf pp "Array[%d]$LEN" id
    | RMemCell { base; prop } -> pf pp "[%Lx].%s" base prop
    | Runknown -> string pp "(unknown reference)"
let show_reference = Fmt.to_to_string pp_reference

module ReferenceSet = struct
  include BatSet.Make(Reference)
  let pp ?sep = Fmt.iter ?sep iter pp_reference
end
module ReferenceMap = struct
  include BatMap.Make(Reference)
  let pp ?sep = Fmt.iter_bindings ?sep iter
  let pp_default ?esep ?psep fmt =
    pp ?sep:esep (Fmt.pair ?sep:psep pp_reference fmt)
end

type js_type = GlobalCode | FunctionCode | EvalCode [@@deriving ord]
let js_type_str = function
    GlobalCode -> "GlobalCode"
  | FunctionCode -> "FunctionCode"
  | EvalCode -> "EvalCode"
let pp_js_type = Fmt.using js_type_str Fmt.string
let show_js_type = Fmt.to_to_string pp_js_type

type event_fire =
  | FireAnchor | FireDefault
  | FireTarget of string
  | FireCapture of string
  | FireBubble of string [@@deriving ord]
let pp_event_fire pp = let open Fmt in function
    | FireAnchor -> string pp "fire:click @ AnchorElement"
    | FireDefault -> string pp "fire:default_handler"
    | FireTarget ev -> pf pp "fire:%s @ TARGET" ev
    | FireCapture ev -> pf pp "fire:%s @ CAPTURE" ev
    | FireBubble ev -> pf pp "fire:%s @ BUBBLE" ev

type event_listener_scope = Add | Remove [@@deriving ord]
let event_listener_scope_str = function
    Add -> "addEventListener"
  | Remove -> "removeEventListener"
let pp_event_listener_scope = Fmt.using event_listener_scope_str Fmt.string

type resource_state =
  | ResLoadStart | ResRecvResp | ResRecvData | ResFinishLoad
  | ResFailed | ResCancelFail | ResAuthRecv | ResCancelAuth
  | MainResponse | ResRecv304 [@@deriving ord]
let resource_state_str = function
  | ResLoadStart -> "loadStart"
  | ResRecvResp -> "recvResp"
  | ResRecvData -> "recvData"
  | ResFinishLoad -> "finishLoad"
  | ResFailed -> "failed"
  | ResCancelFail -> "cancelFail"
  | ResAuthRecv -> "authRecv"
  | ResCancelAuth -> "cancelAuth"
  | MainResponse -> "main response"
  | ResRecv304 -> "recv304"
let pp_resource_state = Fmt.using resource_state_str Fmt.string

type event_handler =
  | EHHittest | EHScroll | EHMousePress | EHMouseMove
  | EHMouseRelease | EHWheel | EHKey [@@deriving ord]
let event_handler_str = function
  | EHHittest -> "eh:hittest"
  | EHScroll -> "eh:scroll"
  | EHMousePress -> "eh:mousepress"
  | EHMouseMove -> "eh:mousemove"
  | EHMouseRelease -> "eh:mouserelease"
  | EHWheel -> "eh:wheel"
  | EHKey -> "eh:key"
let pp_event_handler = Fmt.using event_handler_str Fmt.string

type document_event =
  | DocDelete | DocRecalc | DocPending | DocDOMFocus
  | DOMLayoutTimer [@@deriving ord]
let document_event_str = function
  | DocDelete -> "delete_document"
  | DocRecalc -> "doc style recalc"
  | DocDOMFocus -> "dom focus"
  | DocPending -> "doc pending task" 
  | DOMLayoutTimer -> "layout_timer"
let pp_document_event = Fmt.using document_event_str Fmt.string

type event_queue_scope = EQDispatch | EQLoadEventDelay | EQEventQueue
  [@@deriving ord]
let event_queue_scope_str = function
  | EQDispatch -> "dispatch-event"
  | EQLoadEventDelay -> "load_event_delay"
  | EQEventQueue -> "generic event queue"
let pp_event_queue_scope = Fmt.using event_queue_scope_str Fmt.string

type script_type =
  | STunknown
  | STinline
  | STsync
  | STasync
  | STdefer
  | STIinline
  | STIasync [@@deriving ord]
let script_type_to_string = function
  | STunknown -> "unknown script type"
  | STinline -> "inline script"
  | STsync -> "synchronous external script"
  | STasync -> "async script"
  | STdefer -> "defer script"
  | STIinline -> "inserted inline script"
  | STIasync -> "inserted async script"
let pp_script_type = Fmt.using script_type_to_string Fmt.string
let show_script_type = script_type_to_string

type scope = 
  | JSONDeclareGlobalvar
  | JSONDeclareGlobal
  | JSExec of { callee: int; script_id: int; lstart: int; lend: int;
                source: string; impl: pointer }
  | JSCall of { callee: int; script_id: int; lstart: int; lend: int;
                source: string; impl: pointer }
  | JSCode of { source: string; jstype: js_type }
  | JSInternal of string
  | JSDeclareFunction
  | JSDeclareGlobalvar
  | EventFire of event_fire
  | EventListenerOperation of event_listener_scope
  | ResourceOperation of { state: resource_state; name: string }
  | Event of string
  | Parse of string
  | EventHandler of event_handler
  | DocumentEvent of document_event
  | JSExecuteScript
  | Tokenize
  | TimerScript
  | TimerDOM
  | EventQueue of event_queue_scope
  | CachedResource
  | SUnknown
  | Script of script_type
  | Nondet of string
  | Env of string [@@deriving ord]
let pp_scope pp = let open Fmt in function
    | JSONDeclareGlobalvar -> string pp "json_declare_globalvar"
    | JSONDeclareGlobal -> string pp "json_declare_global"
    | JSDeclareFunction -> string pp "declare_jsfunction"
    | JSDeclareGlobalvar -> string pp "declare_globalvar"
    | JSExecuteScript -> string pp "execute script tag"
    | Tokenize -> string pp "HTML tokenizer"
    | TimerScript -> string pp "script runner timer"
    | TimerDOM -> string pp "DOM Timer"
    | CachedResource -> string pp "cached_resource"
    | JSExec { callee; script_id; lstart; lend; source; impl } ->
      pf pp "Exec (fn=%d #%d) line %d-%d %s [[function:%Lx]]"
        callee script_id lstart lend source impl
    | JSCall { callee; script_id; lstart; lend; source; impl } ->
      pf pp "Call (fn=%d #%d) line %d-%d %s [[function:%Lx]]"
        callee script_id lstart lend source impl
    | JSCode { source; jstype } -> pf pp "%s[%a]" source pp_js_type jstype
    | JSInternal s -> string pp s
    | EventFire f -> pp_event_fire pp f
    | EventListenerOperation s -> pp_event_listener_scope pp s
    | ResourceOperation { state = MainResponse; name } ->
      pf pp "main_reponse %s" name
    | ResourceOperation { state; name } ->
      pf pp "%a:%s" pp_resource_state state name
    | Event e -> pf pp "event:%s" e
    | Parse url -> pf pp "parse HTML[%s]" url
    | EventHandler e -> pp_event_handler pp e
    | DocumentEvent e -> pp_document_event pp e
    | EventQueue e -> pp_event_queue_scope pp e
    | SUnknown -> string pp "(unknown scope)"
    | Script t -> pp_script_type pp t
    | Nondet f -> pf pp "nondet call: %s" f
    | Env f -> pf pp "env-dep. call: %s" f
let show_scope = Fmt.to_to_string pp_scope
let is_javascript_scope = function
  | JSONDeclareGlobal
  | JSONDeclareGlobalvar
  | JSExec _
  | JSCall _
  | JSCode _
  | JSInternal _
  | JSDeclareFunction
  | JSDeclareGlobalvar
  | JSExecuteScript -> true
  | _ -> false

let re s = let open Pcre in regexp ~study:true ~flags:[`ANCHORED] s
let fre f = Format.ksprintf re f
let pointer_of_string s = 
  try Int64.of_string s with _ -> failwith ("Can't parse int64 " ^ s)

let rec match_first s = function
  | [] -> raise Not_found
  | (pat, cont) :: more ->
    try cont (Pcre.exec ~rex:pat s)
    with Not_found -> match_first s more
let match_reference_pats =
  let open Pcre in [
    (re "([\\p{L}\\p{Nl}$_][\\p{L}\\p{Nl}\\p{Nd}\\p{Mn}\\p{Mc}\\p{Pc}$_]*)\\[(-?\\d+)\\]\\.(.*)",
     fun sub -> RHeap {
         objtype = get_substring sub 1;
         id = int_of_string (get_substring sub 2);
         prop = get_substring sub 3
       });
    (re "DOMNode\\[(0x[\\da-fA-F]+)\\]\\.(.*)",
     fun sub -> RDOMNodeAttribute {
         node = pointer_of_string (get_substring sub 1);
         attribute = get_substring sub 2
       });
    (re "NodeTree:(0x[\\da-fA-F]+)",
     fun sub -> RDOMNode (pointer_of_string (get_substring sub 1)));
    (re "Array\\[(-?\\d+)\\]\\$\\[(-?\\d+)\\]",
     fun sub -> RArrayCell {
         id = int_of_string (get_substring sub 1);
         index = int_of_string (get_substring sub 2)
       });
    (re "Array\\[(-?\\d+)\\]\\$LEN",
     fun sub -> RArrayLength (int_of_string (get_substring sub 1)));
    (re "([#A-Za-z\\d]+)\\[(0x[\\da-fA-F]+)\\]\\.(.+)",
     fun sub -> RNodeEvent {
         nodetype = get_substring sub 1;
         node = pointer_of_string (get_substring sub 2);
         handler = get_substring sub 3
       });
    (re "Event\\[(0x[\\da-fA-F]+)\\]",
     fun sub -> REventHandler (pointer_of_string (get_substring sub 1)));
    (re "Timer:(-?\\d+)",
     fun sub -> RTimer (int_of_string (get_substring sub 1)));
    (re "CachedResource-(0x[\\da-fA-F]+)-(0x[\\da-fA-F])",
     fun sub -> RCachedResource {
         resource = pointer_of_string (get_substring sub 1);
         client = pointer_of_string (get_substring sub 2);
       });
    (re "ScriptRunner-(0x[\\da-fA-F]+)-(0x[\\da-fA-F])",
     fun sub -> RScriptRunner {
         runner = pointer_of_string (get_substring sub 1);
         element = pointer_of_string (get_substring sub 2);
       });
    (re "Tree\\[(0x[\\da-fA-F]+)\\]:(.*)",
     fun sub -> RTree {
         scope = pointer_of_string (get_substring sub 1);
         id = get_substring sub 2
       });
    (re "\\[(0x[\\da-fA-F]+)\\]\\.(.*)",
     fun sub -> RMemCell {
         base = pointer_of_string (get_substring sub 1);
         prop = get_substring sub 2
       })

  ]
let match_reference s = match_first s match_reference_pats

let match_value_pats =
  let open Pcre in [
    (re "(-?\\d+)", fun sub -> Vint (Int32.of_string (get_substring sub 1)));
    (re "\"(.*)\"", fun sub -> Vstring (get_substring sub 1));
    (re "([\\p{L}\\p{Nl}$_][\\p{L}\\p{Nl}\\{Nd}\\p{Mn}\\p{Mc}\\p{Pc}$_]*)\\[(-?\\d+)\\]",
     fun sub -> VObject {
         objtype = get_substring sub 1;
         id = int_of_string (get_substring sub 2)
       });
    (re "DOMNode\\[(0x[\\da-fA-F]*)\\]",
     fun sub -> VDOMNode (pointer_of_string (get_substring sub 1)));
    (re "Event\\[(0x[\\da-fA-F]*)\\]",
     fun sub -> VEvent (pointer_of_string (get_substring sub 1)));
    (re "DOMTimer\\[(0x[\\da-fA-F]*)\\]",
     fun sub -> VDOMTimer (pointer_of_string (get_substring sub 1)));
    (re "(.*)",
     fun sub -> begin let s = get_substring sub 1 in
       Vstring s end)
  ]

let match_value s =
  try List.assoc s [
      ("NULL", Vnull);
      ("undefined", Vundefined);
      ("true", Vbool true);
      ("false", Vbool false);
      ("null", Vnullstring) ]
  with Not_found ->
    match_first s match_value_pats

let match_scope_pats = let open Pcre in [
    (re ("Exec \\(fn=(-?\\d+) #(-?\\d+)\\) line (-?\\d+)-(-?\\d+) (.*?) " ^
         "\\[\\[function:(0x[\\da-fA-F]+)\\]\\]"),
     fun sub -> JSExec {
         callee = int_of_string (get_substring sub 1);
         script_id = int_of_string (get_substring sub 2);
         lstart = int_of_string (get_substring sub 3);
         lend = int_of_string (get_substring sub 4);
         source = get_substring sub 5;
         impl = pointer_of_string (get_substring sub 6);
       });
    (re ("Call \\(fn=(-?\\d+) #(-?\\d+)\\) line (-?\\d+)-(-?\\d+) (.*?) " ^
         "\\[\\[function:(0x[\\da-fA-F]+)\\]\\]"),
     fun sub -> JSCall {
         callee = int_of_string (get_substring sub 1);
         script_id = int_of_string (get_substring sub 2);
         lstart = int_of_string (get_substring sub 3);
         lend = int_of_string (get_substring sub 4);
         source = get_substring sub 5;
         impl = pointer_of_string (get_substring sub 6);
       });
    (re ("Exec \\(fn=(-?\\d+) #(-?\\d+)\\) line (-?\\d+)-(-?\\d+) (.*?)"),
     fun sub -> JSExec {
         callee = int_of_string (get_substring sub 1);
         script_id = int_of_string (get_substring sub 2);
         lstart = int_of_string (get_substring sub 3);
         lend = int_of_string (get_substring sub 4);
         source = get_substring sub 5 ^ "...";
	 impl = Int64.of_int 0
       });
    (re ("Call \\(fn=(-?\\d+) #(-?\\d+)\\) line (-?\\d+)-(-?\\d+) (.*?)"),
     fun sub -> JSCall {
         callee = int_of_string (get_substring sub 1);
         script_id = int_of_string (get_substring sub 2);
         lstart = int_of_string (get_substring sub 3);
         lend = int_of_string (get_substring sub 4);
         source = get_substring sub 5 ^ "...";
	 impl = Int64.of_int 0
       });
    (re "JS\\[(.*?)\\]:(.*)",
     fun sub -> JSCode {
         source = get_substring sub 1;
         jstype = List.assoc (get_substring sub 2) [
             ("GlobalCode", GlobalCode);
             ("FunctionCode", FunctionCode);
             ("EvalCode", EvalCode);
           ]
       });
    (re "parse HTML\\[(.*)\\]",
     fun sub -> Parse (get_substring sub 1));
    (re "event:(.*)",
     fun sub -> Event (get_substring sub 1));
    (re "fire:(.*) @ (.*)",
     fun sub -> EventFire (
         match get_substring sub 2 with
         | "TARGET" -> FireTarget (get_substring sub 1)
         | "BUBBLE" -> FireBubble (get_substring sub 1)
         | "CAPTURE" -> FireCapture (get_substring sub 1)
         | _ -> raise Not_found
       ));
    (re "main_response (.*)",
     fun sub ->ResourceOperation {
         state = MainResponse;
         name = get_substring sub 1
       });
    (re "nondet - (.*)",
     fun sub -> Nondet (get_substring sub 1));
    (re "env - (.*)",
     fun sub -> Env (get_substring sub 1));
    (re "auto:.*", fun _ -> failwith "Auto-exploation mode not supported");
    (re "([a-z\\d_]+):(.*)",
     fun sub ->
       let name = get_substring sub 2
       and state = match get_substring sub 1 with
         | "load_start" -> ResLoadStart
         | "recv_resp" -> ResRecvResp
         | "recv_data" -> ResRecvData
         | "finish_load" -> ResFinishLoad
         | "failed" -> ResFailed
         | "cancel_fail" -> ResCancelFail
         | "auth_recv" -> ResAuthRecv
         | "cancel_auth" -> ResCancelAuth
         | "recv_304" -> ResRecv304
         | _ -> raise Not_found
       in ResourceOperation { name; state })
  ]

let match_scope = function
  | "json_declare_globalvar" -> JSONDeclareGlobalvar
  | "json_declare_global" -> JSONDeclareGlobal
  | ( "function:apply" | "array:toString" | "array:toLocaleString"
    | "array:concat" | "array:pop" | "array:push" | "array:reverse"
    | "array:shift" | "array:slice" | "array:sort" | "array:splice"
    | "array:unshift" | "array:filter" | "array:map" | "array:every"
    | "array:foreach" | "array:some" | "array:reduce"
    | "array:reduceRight" | "array:indexOf" | "array:lastIndexOf")
    as internal ->
    JSInternal internal
  | "addEventListener" -> EventListenerOperation Add
  | "removeEventListener" -> EventListenerOperation Remove
  | "declare_jsfunction" -> JSDeclareFunction
  | "declare_globalvar" -> JSDeclareGlobalvar
  | "auto:explore" -> failwith "Use in explore mode is not supported"
  | "fire:click @ AnchorElement" -> EventFire FireAnchor
  | "fire:default_handler" -> EventFire FireDefault
  | "HTML tokenizer" -> Tokenize
  | "execute script tag" -> JSExecuteScript
  | "script runner timer" -> TimerScript
  | "dispatch-event" -> EventQueue EQDispatch
  | "delete_document" -> DocumentEvent DocDelete
  | "doc style recalc" -> DocumentEvent DocRecalc
  | "dom focus" -> DocumentEvent DocDOMFocus
  | "doc pending task" -> DocumentEvent DocPending
  | "load_event_delay" -> EventQueue EQLoadEventDelay
  | "generic event queue" -> EventQueue EQEventQueue
  | "cached_resource" -> CachedResource
  | ("layout_timer"|"layout timer") -> DocumentEvent DOMLayoutTimer
  | "eh:hittest" -> EventHandler EHHittest
  | "eh:scroll" -> EventHandler EHScroll
  | "eh:mousepress" -> EventHandler EHMousePress
  | "eh:mousemove" -> EventHandler EHMouseMove
  | "eh:mouserelease" -> EventHandler EHMouseRelease
  | "eh:wheel" -> EventHandler EHWheel
  | "eh:key" -> EventHandler EHKey
  | "DOM Timer" -> TimerDOM
  | "unknown script" -> Script STunknown
  | "sync script" -> Script STsync
  | "async script" -> Script STasync
  | "defer script" -> Script STdefer
  | "inline script" -> Script STinline
  | "inserted async script" -> Script STIasync
  | "inserted inline script" -> Script STIinline
  | s -> match_first s match_scope_pats

let parse_value = let open EventRacer in function
    | None -> Vunknown
    | Some s ->
      try match_value s with Not_found -> failwith ("Cannot parse value '" ^ s ^ "'")

let parse_reference = let open EventRacer in function
    | None -> Runknown
    | Some s ->
      try match_reference s with Not_found -> failwith ("Cannot parse reference " ^ s)

let parse_scope = let open EventRacer in function
    | None -> SUnknown
    | Some s ->
      try match_scope s with Not_found -> failwith ("Cannot parse scope " ^ s)

type command =
  | Read of reference * value
  | Write of reference * value
  | Post of int
  | Enter of scope
  | Exit
let pp_command pp = let open Fmt in function
    | Read (ref, value) ->
      pf pp "@[<hov>Read %a@ yielding %a@]" pp_reference ref pp_value value
    | Write (ref, value) ->
      pf pp "@[<hov>Write %a@ with %a@]" pp_reference ref pp_value value
    | Post id ->
      pf pp "Post %d" id
    | Enter scope ->
      pf pp "Enter scope %a" pp_scope scope
    | Exit -> string pp "Exit scope"
let show_command = Fmt.to_to_string pp_command
let pp_command_indent pp = let open Fmt in function
    | Read (ref, value) ->
      pf pp "@[<hov>Read %a@ yielding %a@]" pp_reference ref pp_value value
    | Write (ref, value) ->
      pf pp "@[<hov>Write %a@ with %a@]" pp_reference ref pp_value value
    | Post id ->
      pf pp "Post %d" id
    | Enter scope ->
      pf pp "@[<v2>Enter scope %a" pp_scope scope
    | Exit -> pf pp "@]Exit scope"

let parse_command = function
  | CleanLog.Read (r, v) -> Read (parse_reference r, parse_value v)
  | CleanLog.Write (r, v) -> Write (parse_reference r, parse_value v)
  | CleanLog.Enter s -> Enter (parse_scope s)
  | CleanLog.Post p -> Post p
  | CleanLog.Exit -> Exit

type event_action_type =
    EVUnknown | EVTimer | EVNetwork | EVUserInterface | EVContinuation
let event_action_type_str = function
    EVUnknown -> "unknown"
  | EVTimer -> "timer"
  | EVNetwork -> "network"
  | EVUserInterface -> "user interface"
  | EVContinuation -> "continuation"
let pp_event_action_type = Fmt.using event_action_type_str Fmt.string
let show_event_action_type = Fmt.to_to_string pp_event_action_type

type event = {
  evtype: event_action_type;
  id: int;
  commands: command list
}
let pp_event pp { evtype; id; commands } = let open Fmt in
  pf pp "@[<v>Event %d, type %a {@,@[<v2>  %a@]@ }@ @]"
    id
    pp_event_action_type evtype
    (list ~sep:cut pp_command) commands
let show_event = Fmt.to_to_string pp_event

let pp_event_indent pp { evtype; id; commands } = let open Fmt in
  pf pp "@[<v>Event %d, type %a {@,@[<v2>  %a@]@ }@ @]"
    id
    pp_event_action_type evtype
    (list ~sep:cut pp_command_indent) commands

let parse_event { CleanLog.evtype; id; commands } =
  { evtype = (match evtype with
        | EventRacer.Unknown -> EVUnknown
        | EventRacer.Timer -> EVTimer
        | EventRacer.Network -> EVNetwork
        | EventRacer.UserInterface -> EVUserInterface
        | EventRacer.Continuation -> EVContinuation);
    id;
    commands = BatList.map parse_command commands }

module DependencyGraph = CleanLog.DependencyGraph
type race_info = {
  ev1: int;
  ev2: int;
  cmd1: int;
  cmd2: int;
  var: reference
}

type trace = {
  events: event list;
  deps: DependencyGraph.t;
  races: race_info list
}
let pp_trace pp { events; deps } = let open Fmt in
  vbox (list ~sep:Fmt.cut pp_event) pp events
let show_trace = Fmt.to_to_string pp_trace

let pp_pcre_error pp = let open Pcre in let open Fmt in function
  | Partial -> string pp "String only matched the pattern partially"
  | BadPartial -> string pp "Patterns contains iterms that cannot be used together with partial matching"
  | BadPattern (msg, pos) -> pf pp "Regular expression is malformed: %s at %d" msg pos
  | BadUTF8 -> string pp "Invalid UTF8 string"
  | BadUTF8Offset -> string pp "Invalid UTF8 string offset"
  | MatchLimit -> string pp "Maximum allowed number of match attempts with backtracking or recursion reached"
  | RecursionLimit -> string pp "Recursion limit reached"
  | InternalError e -> string pp e

let parse_trace { CleanLog.events; deps; races } =
  Log.debug (fun m -> m "Parsing trace");
  try
      { deps; events = BatList.map parse_event events;
      races =
        let open EventRacer in
          BatList.filter_map
          (fun { ri_event1; ri_event2; ri_cmd1; ri_cmd2; ri_var; ri_covered } ->
              if ri_covered = None then
                  Some { ev1 = ri_event1; ev2 = ri_event2; cmd1 = ri_cmd1;
                  cmd2 = ri_cmd2; var = parse_reference ri_var }
              else None)
          races }
  with Pcre.Error err as e ->
    Log.err (fun m -> m "PCRE error: %a" pp_pcre_error err);
    raise e

let pp_event_with_deps deps pp { evtype; id; commands } =
  let open Fmt in
  pf pp "@[<v>Event %d, type %a, predecessors: @[<hov>%a@],\
         successors: @[<hov>%a@] {@,@[<v2>  %a@]@ }@ @]"
    id
    pp_event_action_type evtype
    (iter ~sep:sp (fun f -> DependencyGraph.iter_pred f deps) int) id
    (iter ~sep:sp (fun f -> DependencyGraph.iter_succ f deps) int) id
    (list ~sep:cut pp_command) commands

let pp_trace_with_deps pp { events; deps } = let open Fmt in
  vbox (list ~sep:Fmt.cut (pp_event_with_deps deps)) pp events
let pp_trace_indent pp { events; deps } = let open Fmt in
  vbox (list ~sep:Fmt.cut (pp_event_indent)) pp events

let load_trace filename =
  filename |> CleanLog.load |> parse_trace
