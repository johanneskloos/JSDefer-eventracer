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

let vertex_attribute classifier more v =
  let c = try IntMap.find v classifier with Not_found -> Other
  in [ `Fillcolor (classify_color c); `Style `Filled ] @ more v

