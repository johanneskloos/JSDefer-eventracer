open ClassifyTask

(* Color scheme:
 * Greens and yellows - top-level scripts.
 * Blue - event handler scripts.
 * Red - unclear events.
 * Gray - non-script content.
 *
 * Inline scripts       Yellow
 * Sync scripts         Lime
 * Async scripts        Light Green
 * Defer scripts        Pale Green
 * Unknown scripts      Orange
 * Immediate eh script  Turquoise
 * Short timer eh scr.  Cyan
 * Long timer eh scr.   Deep Sky Blue
 * Animation eh scr.    Sky Blue
 * ResourceLoad eh scr. Royal Blue
 * Window Interactive s Navy
 * Window Complete s    Blue
 * UI eh script         Power Blue
 * Other eh script      Light Blue
 * Unclear script       Fuchsia
 * HTML parsing         Silver
 * EH without script    Gainsboro
 * Network wait         LightSlateGray
 * Other                Red
 *
 * Additionally, certain kinds of nodes get an
 * "importent" marking by making their font bold.
 * They are: InlineScript, ExternalSyncScript,
 * ExternalUnknownScript, ImmedEventHandlerScript,
 * ShortTimerEventHandlerScript, WindowInteractiveScript,
 * WindowCompleteScript.
 *
 * Furthermore, ExternalSyncScript and ImmedEventHandlerScrpit
 * get a bold border as well.
 *)
let classify_color = function
  | InlineScript -> 0xffff00
  | ExternalSyncScript -> 0x00ff00
  | ExternalAsyncScript -> 0x90EE90
  | ExternalDeferScript -> 0x98FB98
  | ExternalUnknownScript -> 0xFFA500
  | ImmediateEventHandlerScript -> 0x40E0D0
  | ShortTimerEventHandlerScript -> 0x00FFFF
  | LongTimerEventHandlerScript -> 0x00BFFF
  | AnimationEventHandlerScript -> 0x87CEEB
  | ResourceLoadEventHandlerScript -> 0x4169E1
  | WindowInteractiveScript -> 0x000080
  | WindowCompleteScript -> 0x0000FF
  | UIEventHandlerScript -> 0xB0E0E6
  | OtherEventHandlerScript -> 0xADD8E6
  | UnclearScript -> 0xFF00FF
  | HTMLParsing -> 0xC0C0C0
  | EventHandlerNoScript -> 0xDCDCDC
  | NetworkWait -> 0x778899
  | Other -> 0xFF0000
let classify_font = function
  | InlineScript
  | ExternalSyncScript
  | ExternalUnknownScript
  | ImmediateEventHandlerScript
  | ShortTimerEventHandlerScript
  | WindowInteractiveScript
  | WindowCompleteScript -> "Times-12:bold"
  | _ -> "Times-12"
let classify_style = function
  | ExternalSyncScript
  | ImmediateEventHandlerScript -> `Bold
  | _ -> `Solid
let vertex_attribute classifier more v =
  let c = try IntMap.find v classifier with Not_found -> Other
  in [ `Fillcolor (classify_color c); `Style `Filled;
       `Fontname (classify_font c); `Style (classify_style c) ] @ more v

let output_dependency_graph graph classification vertex_more edge channel =
  let module FMT = struct
    include Trace.DependencyGraph
    let graph_attributes (_: t) = []
    let default_vertex_attributes (_: t) = []
    let default_edge_attributes (_: t) = []
    let get_subgraph (_: V.t) = None
    let vertex_name = string_of_int
    let vertex_attributes = vertex_attribute classification vertex_more
    let edge_attributes = edge
  end in
  let module DOT = Graph.Graphviz.Dot(FMT)
  in DOT.output_graph channel graph
let output_post_wait_graph graph classification vertex_more channel =
  let module FMT = struct
    include PostAndWaitGraph.PostWaitGraph
    let graph_attributes (_: t) = []
    let default_vertex_attributes (_: t) = []
    let default_edge_attributes (_: t) = []
    let get_subgraph (_: V.t) = None
    let vertex_name = string_of_int
    let vertex_attributes = vertex_attribute classification vertex_more
    let edge_attributes (_, edgetype, _) = match edgetype with
      | PostAndWaitGraph.PostWaitEdge.POST -> [ `Style `Solid ]
      | PostAndWaitGraph.PostWaitEdge.HB -> [ `Style `Dashed ]
  end in
  let module DOT = Graph.Graphviz.Dot(FMT)
  in DOT.output_graph channel graph

