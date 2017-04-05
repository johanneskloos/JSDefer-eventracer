type classification =
    InlineScript
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
  | Other
  [@@deriving show]

val is_script : classification -> bool
val is_toplevel_script : classification -> bool
val is_event_handler : classification -> bool

val classify : Trace.trace -> Trace.trace * classification IntMap.t
