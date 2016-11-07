open Trace
let encode_value = function
  | Vnull -> Traces_pb.Base_value Traces_pb.Null
  | Vundefined -> Traces_pb.Base_value Traces_pb.Undefined
  | Vnullstring -> Traces_pb.Base_value Traces_pb.Nullstring
  | Vunknown -> Traces_pb.Base_value Traces_pb.Unknown
  | Vbool b -> Traces_pb.Bool_value b
  | Vint i -> Traces_pb.Int_value i
  | Vstring s -> Traces_pb.String_value s
  | VDOMNode p -> Traces_pb.Dom_node_value p
  | VEvent p -> Traces_pb.Event_value p
  | VDOMTimer p -> Traces_pb.Dom_timer_value p
  | VObject { objtype; id } ->
      Traces_pb.(Object_value { objtype; id = Int64.of_int id })
let decode_value = function
  | Traces_pb.Base_value Traces_pb.Null -> Vnull
  | Traces_pb.Base_value Traces_pb.Undefined -> Vundefined
  | Traces_pb.Base_value Traces_pb.Nullstring -> Vnullstring
  | Traces_pb.Base_value Traces_pb.Unknown -> Vunknown
  | Traces_pb.Bool_value b -> Vbool b
  | Traces_pb.Int_value i -> Vint i
  | Traces_pb.String_value s -> Vstring s
  | Traces_pb.Dom_node_value p -> VDOMNode p
  | Traces_pb.Event_value p -> VEvent p
  | Traces_pb.Dom_timer_value p -> VDOMTimer p
  | Traces_pb.Object_value { Traces_pb.objtype; id } ->
      VObject { objtype; id = Int64.to_int id }

type typed_field_pointer = {
  type_ : string;
  field : string;
  pointer : int64;
}

type field_pointer = {
  field : string;
  pointer : int64;
}

type pointer_pair = {
  first : int64;
  second : int64;
}

type reference =
  | Scriptrunner of pointer_pair
  | Eventhandler of int64
  | Tree of field_pointer
  | Domnodeattribute of field_pointer
  | Timer of int32
  | Nodeevent of typed_field_pointer
  | Array_cell of pointer_pair
  | Array_length of int64
  | Mem_cell of field_pointer
  | Unknown of int32
let encode_reference = function
  | RHeap { objtype; id; prop } ->
      Traces_pb.(Heap { type_ = objtype;
                        field = prop;
                        pointer = Int64.of_int id })
  | RDOMNode p -> Traces_pb.Domnode p
  | RScriptRunner { runner; element } ->
      Traces_pb.(Scriptrunner { first = runner; second = element })
  | REventHandler p -> Traces_pb.Eventhandler p
  | RTree { scope; id } -> Traces_pb.(Tree { field = id; pointer = scope })
  | RDOMNodeAttribute { node; attribute } ->
      Traces_pb.(Domnodeattribute { field = attribute; pointer = node })
  | RCachedResource { resource; client } ->
      Traces_pb.(Cached_resource { first = resource; second = client })
  | RTimer t -> Traces_pb.Timer (Int32.of_int t)
  | RNodeEvent { nodetype; node; handler } ->
      Traces_pb.(Nodeevent { type_ = nodetype;
                             field = handler;
                             pointer = node })
  | RArrayCell { id; index } ->
      Traces_pb.(Array_cell { first = Int64.of_int id;
                              second = Int64.of_int index })
  | RArrayLength id ->
      Traces_pb.Array_length (Int64.of_int id)
  | RMemCell { base; prop } ->
      Traces_pb.(Mem_cell { field = prop; pointer = base })
  | Runknown -> Traces_pb.Unknown Int32.zero
let decode_reference = function
  | Traces_pb.Heap { Traces_pb.type_; field; pointer } ->
      RHeap { objtype = type_; prop = field; id = Int64.to_int pointer }
  | Traces_pb.Domnode p -> RDOMNode p
  | Traces_pb.Scriptrunner { Traces_pb.first; second } ->
      RScriptRunner { runner = first; element = second }
  | Traces_pb.Eventhandler p -> REventHandler p
  | Traces_pb.Tree { Traces_pb.field; pointer } ->
      RTree { scope = pointer; id = field }
  | Traces_pb.Domnodeattribute { Traces_pb.field; pointer } ->
      RDOMNodeAttribute { node = pointer; attribute = field }
  | Traces_pb.Cached_resource { Traces_pb.first; second } ->
      RCachedResource { resource = first; client = second }
  | Traces_pb.Timer t -> RTimer (Int32.to_int t)
  | Traces_pb.Nodeevent { Traces_pb.type_; field; pointer } ->
      RNodeEvent { nodetype = type_; handler = field; node = pointer }
  | Traces_pb.Array_cell { Traces_pb.first; second } ->
      RArrayCell { id = Int64.to_int first; index = Int64.to_int second }
  | Traces_pb.Array_length id ->
      RArrayLength (Int64.to_int id)
  | Traces_pb.Mem_cell { Traces_pb.field; pointer } ->
      RMemCell { base = pointer; prop = field }
  | Traces_pb.Unknown _ -> Runknown

let encode_js_type = function
  | GlobalCode -> Traces_pb.Global_code
  | FunctionCode -> Traces_pb.Function_code
  | EvalCode -> Traces_pb.Eval_code

let decode_js_type = function
  | Traces_pb.Global_code -> GlobalCode
  | Traces_pb.Function_code -> FunctionCode
  | Traces_pb.Eval_code -> EvalCode

let encode_event_fire = function
  | FireAnchor -> Traces_pb.Plain_event Traces_pb.Ev_anchor
  | FireDefault -> Traces_pb.Plain_event Traces_pb.Ev_default
  | FireTarget name -> Traces_pb.(Named_event { mode = Target; name })
  | FireCapture name -> Traces_pb.(Named_event { mode = Capture; name })
  | FireBubble name -> Traces_pb.(Named_event { mode = Bubble; name })
let encode_resource_state name = function
  | ResLoadStart ->
      Traces_pb.(Resource_operation { state = Res_load_start; name })
  | ResRecvResp ->
      Traces_pb.(Resource_operation { state = Res_recv_resp; name })
  | ResRecvData ->
      Traces_pb.(Resource_operation { state = Res_recv_data; name })
  | ResFinishLoad ->
      Traces_pb.(Resource_operation { state = Res_finish_load; name })
  | ResFailed ->
      Traces_pb.(Resource_operation { state = Res_failed; name })
  | ResCancelFail ->
      Traces_pb.(Resource_operation { state = Res_cancel_fail; name })
  | ResAuthRecv ->
      Traces_pb.(Resource_operation { state = Res_auth_recv; name })
  | ResCancelAuth ->
      Traces_pb.(Resource_operation { state = Res_cancel_auth; name })
  | MainResponse ->
      Traces_pb.(Resource_operation { state = Res_main_response; name })
  | ResRecv304 ->
      Traces_pb.(Resource_operation { state = Res_recv_304; name })
let encode_event_handler = function
  | EHHittest -> Traces_pb.(Plain_event Eh_hittest)
  | EHScroll -> Traces_pb.(Plain_event Eh_scroll)
  | EHMousePress -> Traces_pb.(Plain_event Eh_mouse_press)
  | EHMouseMove -> Traces_pb.(Plain_event Eh_mouse_move)
  | EHMouseRelease -> Traces_pb.(Plain_event Eh_mouse_release)
  | EHWheel -> Traces_pb.(Plain_event Eh_wheel)
  | EHKey -> Traces_pb.(Plain_event Eh_key)
let encode_document_event = function
  | DocDelete -> Traces_pb.(Plain_event Doc_delete)
  | DocRecalc -> Traces_pb.(Plain_event Doc_recalc)
  | DocPending -> Traces_pb.(Plain_event Doc_pending)
  | DocDOMFocus -> Traces_pb.(Plain_event Doc_dom_focus)
  | DOMLayoutTimer -> Traces_pb.(Plain_event Doc_dom_layout)
let encode_event_queue_scope = function
  | EQDispatch -> Traces_pb.(Plain_event Eq_dispath)
  | EQLoadEventDelay -> Traces_pb.(Plain_event Eq_load_delay)
  | EQEventQueue -> Traces_pb.(Plain_event Eq_queue)
let encode_scope = function
  | JSONDeclareGlobalvar -> Traces_pb.(Basic_scope Json_declare_global_var)
  | JSONDeclareGlobal -> Traces_pb.(Basic_scope Json_declare_global)
  | JSDeclareGlobalvar -> Traces_pb.(Basic_scope Js_declare_global_var)
  | JSDeclareFunction -> Traces_pb.(Basic_scope Js_declare_function)
  | JSExecuteScript -> Traces_pb.(Basic_scope Js_execute_script)
  | Tokenize -> Traces_pb.(Basic_scope Tokenize)
  | TimerScript -> Traces_pb.(Basic_scope Timer_script)
  | TimerDOM -> Traces_pb.(Basic_scope Timer_dom)
  | CachedResource -> Traces_pb.(Basic_scope Cached_resource)
  | SUnknown -> Traces_pb.(Basic_scope Unknown)
  | EventFire e -> encode_event_fire e
  | ResourceOperation { state; name } -> encode_resource_state name state
  | EventHandler eh -> encode_event_handler eh
  | DocumentEvent dh -> encode_document_event dh
  | EventQueue eq -> encode_event_queue_scope eq
  | JSInternal name -> Traces_pb.(Js_internal name)
  | Event name -> Traces_pb.(Event name)
  | Parse name -> Traces_pb.(Parse name)
  | JSExec { callee; script_id; lstart; lend; source; impl } ->
      Traces_pb.(Js_exec { callee = Int32.of_int callee;
                           script_id = Int32.of_int script_id;
                           lstart = Int32.of_int lstart;
                           lend = Int32.of_int lend;
                           source; pointer = impl })
  | JSCall { callee; script_id; lstart; lend; source; impl } ->
      Traces_pb.(Js_call { callee = Int32.of_int callee;
                           script_id = Int32.of_int script_id;
                           lstart = Int32.of_int lstart;
                           lend = Int32.of_int lend;
                           source; pointer = impl })
  | JSCode { source; jstype } ->
      Traces_pb.Js_code { Traces_pb.source;
                          type_ = encode_js_type jstype }
  | EventListenerOperation Add ->
      Traces_pb.(Basic_scope Listener_add)
  | EventListenerOperation Remove ->
      Traces_pb.(Basic_scope Listener_remove)
let decode_plain_event_types = function
  | Traces_pb.Ev_anchor -> EventFire FireAnchor
  | Traces_pb.Ev_default -> EventFire FireDefault
  | Traces_pb.Eh_hittest -> EventHandler EHHittest
  | Traces_pb.Eh_scroll -> EventHandler EHScroll
  | Traces_pb.Eh_mouse_press -> EventHandler EHMousePress
  | Traces_pb.Eh_mouse_move -> EventHandler EHMouseMove
  | Traces_pb.Eh_mouse_release -> EventHandler EHMouseRelease
  | Traces_pb.Eh_wheel -> EventHandler EHWheel
  | Traces_pb.Eh_key -> EventHandler EHKey
  | Traces_pb.Doc_delete -> DocumentEvent DocDelete
  | Traces_pb.Doc_recalc -> DocumentEvent DocRecalc
  | Traces_pb.Doc_pending -> DocumentEvent DocPending
  | Traces_pb.Doc_dom_focus -> DocumentEvent DocDOMFocus
  | Traces_pb.Doc_dom_layout -> DocumentEvent DOMLayoutTimer
  | Traces_pb.Eq_dispath -> EventQueue EQDispatch
  | Traces_pb.Eq_load_delay -> EventQueue EQLoadEventDelay
  | Traces_pb.Eq_queue -> EventQueue EQEventQueue
let decode_res_event_type name = function
  | Traces_pb.Res_load_start -> ResourceOperation { name; state = ResLoadStart }
  | Traces_pb.Res_recv_resp  -> ResourceOperation { name; state = ResRecvResp }
  | Traces_pb.Res_recv_data  -> ResourceOperation { name; state = ResRecvData }
  | Traces_pb.Res_finish_load  -> ResourceOperation { name; state = ResFinishLoad }
  | Traces_pb.Res_failed  -> ResourceOperation { name; state = ResFailed }
  | Traces_pb.Res_cancel_fail  -> ResourceOperation { name; state = ResCancelFail }
  | Traces_pb.Res_auth_recv  -> ResourceOperation { name; state = ResAuthRecv }
  | Traces_pb.Res_cancel_auth  -> ResourceOperation { name; state = ResCancelAuth }
  | Traces_pb.Res_main_response  -> ResourceOperation { name; state = MainResponse }
  | Traces_pb.Res_recv_304  -> ResourceOperation { name; state = ResRecv304 }
let decode_basic_scopes = function
  | Traces_pb.Json_declare_global_var -> JSONDeclareGlobalvar
  | Traces_pb.Json_declare_global -> JSONDeclareGlobal
  | Traces_pb.Js_declare_global_var -> JSDeclareGlobalvar
  | Traces_pb.Js_declare_function -> JSDeclareFunction
  | Traces_pb.Js_execute_script -> JSExecuteScript
  | Traces_pb.Tokenize -> Tokenize
  | Traces_pb.Timer_script -> TimerScript
  | Traces_pb.Timer_dom -> TimerDOM
  | Traces_pb.Cached_resource -> CachedResource
  | Traces_pb.Unknown -> SUnknown
  | Traces_pb.Listener_add -> EventListenerOperation Add
  | Traces_pb.Listener_remove -> EventListenerOperation Remove
let decode_scope = function
  | Traces_pb.Basic_scope s -> decode_basic_scopes s
  | Traces_pb.Plain_event e -> decode_plain_event_types e
  | Traces_pb.Named_event { Traces_pb.name; mode=Traces_pb.Target } ->
      EventFire (FireTarget name)
  | Traces_pb.Named_event { Traces_pb.name; mode=Traces_pb.Bubble } ->
      EventFire (FireBubble name)
  | Traces_pb.Named_event { Traces_pb.name; mode=Traces_pb.Capture } ->
      EventFire (FireCapture name)
  | Traces_pb.Js_internal name -> JSInternal name
  | Traces_pb.Event name -> Event name
  | Traces_pb.Parse name -> Parse name
  | Traces_pb.Js_exec { Traces_pb.callee; script_id; lstart; lend;
                        source; pointer } ->
      JSExec { callee = Int32.to_int callee;
               script_id = Int32.to_int script_id;
               lstart = Int32.to_int lstart;
               lend = Int32.to_int lend;
               source; impl = pointer }
  | Traces_pb.Js_call { Traces_pb.callee; script_id; lstart; lend;
                        source; pointer } ->
      JSCall { callee = Int32.to_int callee;
               script_id = Int32.to_int script_id;
               lstart = Int32.to_int lstart;
               lend = Int32.to_int lend;
               source; impl = pointer }
  | Traces_pb.Js_code { Traces_pb.source; type_ } ->
      JSCode { source; jstype = decode_js_type type_ }
  | Traces_pb.Resource_operation { Traces_pb.name; state } ->
      decode_res_event_type name state

let encode_command = function
  | Read (ref, value) ->
      Traces_pb.Read { Traces_pb.ref = encode_reference ref;
                       val_ = encode_value value }
  | Write (ref, value) ->
      Traces_pb.Write { Traces_pb.ref = encode_reference ref;
                        val_ = encode_value value }
  | Post id ->
      Traces_pb.(Post (Int32.of_int id))
  | Enter scope ->
      Traces_pb.Enter (encode_scope scope)
  | Exit ->
      Traces_pb.(Exit Int32.zero)
let decode_command = function
  | Traces_pb.Read { Traces_pb.ref; val_ } ->
      Read (decode_reference ref, decode_value val_)
  | Traces_pb.Write { Traces_pb.ref; val_ } ->
      Write (decode_reference ref, decode_value val_)
  | Traces_pb.Post id ->
      Post (Int32.to_int id)
  | Traces_pb.Enter scope ->
      Enter (decode_scope scope)
  | Traces_pb.Exit _ ->
      Exit

let encode_event_action_type = function
  | EVUnknown -> Traces_pb.Unknown
  | EVTimer -> Traces_pb.Timer
  | EVNetwork -> Traces_pb.Network
  | EVUserInterface -> Traces_pb.Ui
  | EVContinuation -> Traces_pb.Continuation

let decode_event_action_type = function
  | Traces_pb.Unknown -> EVUnknown
  | Traces_pb.Timer -> EVTimer
  | Traces_pb.Network -> EVNetwork
  | Traces_pb.Ui -> EVUserInterface
  | Traces_pb.Continuation -> EVContinuation

let encode_event { evtype; id; commands } =
  { Traces_pb.evtype = encode_event_action_type evtype;
    id = Int32.of_int id;
    commands = BatList.map encode_command commands }
let decode_event { Traces_pb.evtype; id; commands } =
  { evtype = decode_event_action_type evtype;
    id = Int32.to_int id;
    commands = BatList.map decode_command commands }

let encode_graph deps =
  let nodes =
    DependencyGraph.fold_vertex
      (fun v nodes -> Int32.of_int v :: nodes)
      deps []
  and edges =
    DependencyGraph.fold_edges_e
      (fun (src, delay, tgt) edges ->
         { Traces_pb.src = Int32.of_int src;
           tgt = Int32.of_int tgt;
           delay = BatOption.map Int32.of_int delay } :: edges)
      deps []
  in { Traces_pb.nodes; edges }
let decode_graph { Traces_pb.nodes; edges } =
  let graph =
    BatList.fold_left (fun g v ->
                         DependencyGraph.add_vertex g (Int32.to_int v))
      DependencyGraph.empty nodes
  in BatList.fold_left (fun g { Traces_pb.src; tgt; delay } ->
                          DependencyGraph.add_edge_e g
                            (Int32.to_int src,
                             BatOption.map Int32.to_int delay,
                             Int32.to_int tgt))
       graph edges

let encode_trace { events; deps } =
  { Traces_pb.events = BatList.map encode_event events;
    graph = encode_graph deps }
let decode_trace { Traces_pb.events; graph } =
  { events = BatList.map decode_event events;
    deps = decode_graph graph }

