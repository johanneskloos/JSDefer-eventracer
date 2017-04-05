let source = ref Logs.default
let set_source_for_file filename =
  source := Logs.Src.create filename

let improved_reporter () =
  let logfile =
    open_out "log.txt"
  in at_exit (fun () -> close_out logfile);
     let report (src: Logs.src) (level: Logs.level) ~over k msgf =
       let k _ = over (); k () in
         msgf @@ fun ?header ?tags fmt ->
         Format.kasprintf (fun txt ->
                            output_string logfile 
                              (Logs.level_to_string (Some level) ^ ":" ^
                               Logs.Src.name src ^ ":" ^
                               BatString.quote txt ^ "\n");
                            Format.kfprintf k Fmt.stdout ("%a %s: @[%s@]@.")
                              Logs.pp_header (level, header)
                              (Logs.Src.name src)
                              txt)
           fmt
     in { Logs.report }

let () =
  Logs.set_reporter (improved_reporter ());
  Logs.set_level ~all:true (Some Logs.Info);

