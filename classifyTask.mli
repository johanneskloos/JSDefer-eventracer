(** Classify the events of a trace. *)

(** The classification of a trace event. *)
type classification =
    InlineScript (** An inline script *)
  | ExternalSyncScript (** An external script, loaded synchronously *)
  | ExternalAsyncScript (** An external script, loaded with [async] *)
  | ExternalDeferScript (** An external script, loaded with [defer] *)
  | ExternalUnknownScript (** An external script, loaded some other way *)
  | ImmediateEventHandlerScript
  (** An event handler that fires with no delay *)
  | ShortTimerEventHandlerScript
  (** An event handler that fires with short delay (cutoff 10ms) *)
  | LongTimerEventHandlerScript
  (** An event handler that fires with long delay (cutoff 10ms) *)
  | AnimationEventHandlerScript
  (** An event hander for animation (common enough to disambiguate) *)
  | ResourceLoadEventHandlerScript
  (** An event hander for resource loads *)
  | WindowInteractiveScript
  (** The ``window interactive'' event. XXX is this DCL or onLoad? *)
  | WindowCompleteScript
  (** The ``window complete'' event. XXX is this DCL or onLoad? *)
  | UIEventHandlerScript
  (** An event handler call for UI events. *)
  | OtherEventHandlerScript
  (** An event handler call something else. *)
  | UnclearScript
  (** JavaScript execution, with unclear function *)
  | HTMLParsing
  (** HTML parsing step *)
  | EventHandlerNoScript
  (** Event handler not involving JavaScript *)
  | NetworkWait
  (** Waiting for the network *)
  | Other
      (** Some other, non-JavaScript event *)
      [@@deriving show]

(** Classification for each event in a trace. *)
type trace_classifications = classification IntMap.t

(** Does this classification indicate a JavaScript script is running? *)
val is_script : classification -> bool

(** Does this classification indicate a top-level JavaScript script
    is running? *)
val is_toplevel_script : classification -> bool

(** Is this an event handler? *)
val is_event_handler : classification -> bool

(** Classify the events in a given trace [s]. It returns
  a subtrace of [s], with all ``junk events'' (e.g., empty events)
  removed, and a classification for event in the trace. *)
val classify : Trace.trace -> Trace.trace * trace_classifications
