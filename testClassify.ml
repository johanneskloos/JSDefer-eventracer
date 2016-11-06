open Trace

let pp_event_classified classification pp { evtype; id; commands } =
  let open ClassifyTask
  in let evclass = try IntMap.find id classification with Not_found -> Other
  in let open Fmt
  in pf pp "@[<v>Event %d, type %a, class %a {@,@[<v2>  %a@]@,}@,@]"
       id pp_event_action_type evtype pp_classification evclass
       (list ~sep:cut pp_command) commands

let analyze file =
  let ({ events }, classify) =
    file |> CleanLog.load |> Trace.parse_trace |> ClassifyTask.classify
  in let open Fmt in
    Format.printf "@[<v>%s:@,@,%a@,@]"
      file
      (list ~sep:cut (pp_event_classified classify)) events

let () = Arg.parse [] analyze ""
