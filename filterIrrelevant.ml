(* Trace filtering criteria:
   We classify events into
   "interesting" and "uninteresting".
   Interesting events are:
   - all events that include JavaScript code,
   - all events that acesss the JavaScript heap,
   - all events that access JavaScript-written values.
   Regardless, an event is always uninteresting if it
   happens after the window onLoad event.
*)
let check_
