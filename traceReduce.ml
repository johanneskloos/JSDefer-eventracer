open Trace

let rec remove_before_page_impl = function
  | { commands } as event :: events ->
      if not (BatList.exists (function
                                | Enter (Parse "") -> false
                                | Enter (Parse _) -> true
                                | _ -> false)
                commands)
      then remove_before_page_impl events
      else event :: events
  | [] -> []

let remove_before_page { events; deps } =
  { events = remove_before_page_impl events; deps }

let remove_empty { events; deps } =
  { events = BatList.filter (function
                               | { commands = [] } -> false
                               | _ -> true)
               events;
    deps }

let (%) = BatPervasives.(%)

let cleanup = remove_before_page % remove_empty
