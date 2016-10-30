open Trace
open ClassifyTask

let show_classification filename =
  let { events } as trace = filename |> CleanLog.load |> Trace.parse_trace in
  let dom_timer_handlers = find_dom_timer_handlers trace
  and animation_requests = find_animation_requests trace
  in let animation_handlers = find_animation_handlers animation_requests trace
  in
    Format.printf "@[<v2>%s:@,@[<hov2>DOM timer handlers: %a@]@,%a@]@,"
      filename
      (IntSet.pp ~sep:Fmt.sp) dom_timer_handlers
      Fmt.(list ~sep:cut (using (classify_event dom_timer_handlers animation_handlers)
                            (pair ~sep:(const string ":")
                               int pp_classification)))
      events

let () =
  Arg.parse [] show_classification "..."
