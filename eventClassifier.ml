open Trace

let is_page_onload { commands } =
  (* Checking for the load event is tricky - it can get fired by other
     elements as well. But pageshow is window-only, and webkit fires
     them both in one go. *)
  BatList.mem (Enter (Event "showpage")) commands

