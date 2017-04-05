type pointer = int64 [@@deriving ord]

type value =
    Vnull
  | Vundefined
  | Vbool of bool
  | Vint of int32
  | Vnullstring
  | Vstring of string
  | VDOMNode of pointer
  | VObject of { objtype : string; id : int; }
  | VEvent of pointer
  | VDOMTimer of pointer
  | Vunknown
  [@@deriving ord, show]
module ValueSet : BatSet.S with type elt = value
module ValueMap : sig
  include BatMap.S with  type key = value
  val pp: ?sep:(unit Fmt.t) -> (value * 'a) Fmt.t -> 'a t Fmt.t
  val pp_default: ?esep:(unit Fmt.t) ->
    ?psep:(unit Fmt.t) -> 'a Fmt.t -> 'a t Fmt.t
end

type reference =
    RHeap of { objtype : string; id : int; prop : string; }
  | RDOMNode of pointer
  | RScriptRunner of { runner : pointer; element : pointer; }
  | REventHandler of pointer
  | RTree of { scope : pointer; id : string; }
  | RDOMNodeAttribute of { node : pointer; attribute : string; }
  | RCachedResource of { resource : pointer; client : pointer; }
  | RTimer of int
  | RNodeEvent of { nodetype : string; node : pointer; handler : string; }
  | RArrayCell of { id : int; index : int; }
  | RArrayLength of int
  | RMemCell of { base : pointer; prop : string; }
  | Runknown
  [@@deriving ord, show]
module ReferenceSet : BatSet.S with type elt = reference
module ReferenceMap : sig
  include BatMap.S with type key = reference
  val pp: ?sep:(unit Fmt.t) -> (reference * 'a) Fmt.t -> 'a t Fmt.t
  val pp_default: ?esep:(unit Fmt.t) ->
    ?psep:(unit Fmt.t) -> 'a Fmt.t -> 'a t Fmt.t
end

type js_type = GlobalCode | FunctionCode | EvalCode [@@deriving ord, show]

type event_fire =
    FireAnchor
  | FireDefault
  | FireTarget of string
  | FireCapture of string
  | FireBubble of string

type event_listener_scope = Add | Remove [@@deriving ord]

type resource_state =
    ResLoadStart
  | ResRecvResp
  | ResRecvData
  | ResFinishLoad
  | ResFailed
  | ResCancelFail
  | ResAuthRecv
  | ResCancelAuth
  | MainResponse
  | ResRecv304

type event_handler =
    EHHittest
  | EHScroll
  | EHMousePress
  | EHMouseMove
  | EHMouseRelease
  | EHWheel
  | EHKey

type document_event =
    DocDelete
  | DocRecalc
  | DocPending
  | DocDOMFocus
  | DOMLayoutTimer

type event_queue_scope = EQDispatch | EQLoadEventDelay | EQEventQueue

type script_type =
    STunknown
  | STinline
  | STsync
  | STasync
  | STdefer
  | STIinline
  | STIasync
  [@@deriving ord, show]

type scope =
    JSONDeclareGlobalvar
  | JSONDeclareGlobal
  | JSExec of { callee : int; script_id : int; lstart : int; lend : int;
      source : string; impl : pointer;
    }
  | JSCall of { callee : int; script_id : int; lstart : int; lend : int;
      source : string; impl : pointer;
    }
  | JSCode of { source : string; jstype : js_type; }
  | JSInternal of string
  | JSDeclareFunction
  | JSDeclareGlobalvar
  | EventFire of event_fire
  | EventListenerOperation of event_listener_scope
  | ResourceOperation of { state : resource_state; name : string; }
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
  | Env of string
  [@@deriving ord, show]

val is_javascript_scope : scope -> bool

type command =
    Read of reference * value
  | Write of reference * value
  | Post of int
  | Enter of scope
  | Exit
  [@@deriving show]

val pp_command_indent : Format.formatter -> command -> unit

type event_action_type =
    EVUnknown
  | EVTimer
  | EVNetwork
  | EVUserInterface
  | EVContinuation
  [@@deriving show]

type event = {
  evtype : event_action_type;
  id : int;
  commands : command list;
} [@@deriving show]

val pp_event_indent : Format.formatter -> event -> unit

module DependencyGraph = CleanLog.DependencyGraph

type race_info = {
  ev1 : int;
  ev2 : int;
  cmd1 : int;
  cmd2 : int;
  var : reference;
}

type trace = {
  events : event list;
  deps : DependencyGraph.t;
  races : race_info list;
} [@@deriving show]
val pp_trace : Format.formatter -> trace -> unit


val pp_event_with_deps : DependencyGraph.t -> event Fmt.t
val pp_trace_with_deps : trace Fmt.t
val pp_trace_indent : trace Fmt.t
val parse_trace : CleanLog.trace -> trace
val load_trace : string -> trace
