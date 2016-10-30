open Trace

let collect_scopes { id; commands } =
  let rec collect (stack, closed) = function
    | Enter (JSCode { jstype = GlobalCode }) :: _ ->
        Some (stack, BatList.sort_unique compare_scope closed)
    | Enter scope :: commands ->
        collect (scope :: stack, closed) commands
    | Exit :: commands ->
        begin match stack with
          | scope :: stack -> collect (stack, scope :: closed) commands
          | [] -> failwith "Bad bracketing"
        end
    | _ :: commands -> collect (stack, closed) commands
    | [] -> None
  in match collect ([], []) commands with
    | Some (stack, closed) ->
        Format.printf "@[<v>Event %d: @[<hov>%a@], closed: @[<hov>%a@]@,@]"
          id Fmt.(list ~sep:sp pp_scope) stack Fmt.(list ~sep:sp pp_scope) closed
    | None -> ()

let scope_analysis filename =
  let { events } = filename |> CleanLog.load |> Trace.parse_trace
  in Format.printf "@[<v>%s:@,@]" filename;
     List.iter collect_scopes events;
     Format.print_cut ()

let () = Arg.parse [] scope_analysis "..."
