open Trace

let show_url base { id; commands } =
  let rec find_url = function
    | Enter (JSCode { source }) :: _ ->
        Format.printf "%s\t%d\t%s@," base id source
    | _ :: rest -> find_url rest
    | [] -> ()
  in let rec check_if_script = function
    | Enter (Script STinline) :: _
    | Enter (Script STIinline) :: _ ->
        Format.printf "%s\t%d\t(inline script)@," base id
    | Enter (Script s) :: rest ->
        find_url rest
    | _ :: rest -> check_if_script rest
    | _ -> ()
  in check_if_script commands

let show_urls file =
  match Unix.fork () with
    | 0 ->
        Format.open_vbox 0;
        List.iter (show_url (Filename.chop_suffix (Filename.basename file) ".log"))
          (load_trace file).events;
        Format.close_box ();
        exit 0
    | pid -> Unix.waitpid [] pid |> ignore

let () =
  Arg.parse [] show_urls "";
