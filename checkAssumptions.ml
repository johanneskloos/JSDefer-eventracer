open Trace

let assumption_post_included_in_hb filename { deps; events } =
  List.iter
    (fun { id = src; commands } ->
       List.iter
         (function
            | Post (-1) -> ()
            | Post dst ->
                if not (DependencyGraph.mem_edge deps src dst) then begin
                  Format.printf "In %s: Post %d -> %d not in hb@,"
                    filename src dst;
                  DependencyGraph.iter_pred_e
                    (function
                       | (src', Some duration, _) ->
                           Format.printf "  There is %d -(%d)-> %d@,"
                             src' duration dst
                       | (src', None, _) ->
                           Format.printf "  There is %d -> %d@,"
                             src' dst)
                    deps dst;
                  if BatList.for_all
                       (function
                          | Post _ -> true
                          | _ -> false)
                       commands then
                    Format.printf "  Event %d is a post cluster@," src
                end
            | _ -> ())
         commands)
    events

let assumption_times_clearly_posted filename { events; deps } =
  DependencyGraph.iter_edges_e
    (function (src, Some _, tgt) ->
      let rec has_clear_post = function
        | Enter TimerDOM ::
          Post tgt' ::
          Write (RTimer _, VDOMTimer _) ::
          Exit :: _ when tgt = tgt' -> true
        | _ :: rest -> has_clear_post rest
        | [] -> false
      in let { id; commands } = List.nth events src
      in if id <> src then failwith "Inconsistency"
      else if not (has_clear_post commands) then
        Format.printf "%s: %d -> %d does not have clear timer post@,"
          filename src tgt
       | _ -> ())
    deps

let check_assumptions filename =
  let trace = filename |> CleanLog.load |> Trace.parse_trace in
    assumption_post_included_in_hb filename trace;
    assumption_times_clearly_posted filename trace

let () =
  Format.pp_open_vbox Format.std_formatter 0;
  Arg.parse [] check_assumptions "...";
  Format.pp_print_cut Format.std_formatter ();
  Format.pp_close_box Format.std_formatter ()

