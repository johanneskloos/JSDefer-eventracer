open Trace

let trace_cache = Hashtbl.create 31

let load_trace_if_needed hostname =
  try
    Hashtbl.find trace_cache hostname
  with Not_found ->
    let trace = load_trace (hostname ^ ".log")
    in Hashtbl.add trace_cache hostname trace;
       trace

let find_url hostname script { commands } =
  List.iter (function
               | Enter (JSCode { source }) ->
                   Format.printf "%s:%d:%s@," hostname script source
               | _ -> ())
    commands

let show_url hostname script =
  let { events } = load_trace_if_needed hostname in
    find_url hostname script (List.nth events script)

let colon = Str.regexp ":"

let show_urls file =
  let cin = open_in file in try
    let rec loop () =
      let line = input_line cin
      in match Str.split colon line with
        | [ hostname; script ] ->
            show_url hostname (int_of_string script);
            loop ()
        | _ -> Format.printf "Can't parse %s@," line;
               raise Exit
    in loop ()
  with End_of_file -> close_in cin
    | e -> close_in cin; raise e

let () =
  Format.open_vbox 0;
  Arg.parse [] show_urls "";
  Format.close_box ()
