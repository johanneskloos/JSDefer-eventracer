let with_out_file filename f =
  let chan = open_out filename in try
    f chan;
    close_out chan
  with e ->
    close_out chan;
    raise e

let with_formatter filename f =
  with_out_file filename
    (fun chan ->
       let pp = Format.formatter_of_out_channel chan in
         Format.pp_open_vbox pp 0;
         f pp;
         Format.pp_close_box pp ();
         Format.pp_print_flush pp ())

let write_to_file filename (fmt: 'a Fmt.t) data =
  with_formatter filename (fun pp -> fmt pp data)

let load_determinism_facts filename =
  try
    BatFile.lines_of filename |> BatEnum.fold
      (fun known_det str -> IntSet.add (int_of_string str) known_det)
      IntSet.empty
  with _ -> IntSet.empty

let calculate_and_write_analysis log use_det base intrace indet makeoutput =
  if !log then DetailLog.open_log (open_out (makeoutput ".details"));
  let deterministic_scripts =
    if use_det && Sys.file_exists indet then
      load_determinism_facts indet
    else
      IntSet.empty
  in let summary =
    CleanLog.load intrace
    |> Trace.parse_trace
    |> Deferability.calculate_deferability deterministic_scripts
    |> Summary.summarize base deterministic_scripts
  in
    write_to_file (makeoutput "result") Summary.pp_page_summary summary;
    with_out_file (makeoutput "result.csv") (Summary.csv_page_summary summary);
    write_to_file (makeoutput "defer") Summary.pp_defer summary

let analyze log filename use_determinism_facts =
  let open Filename in
    if Sys.is_directory filename then
      calculate_and_write_analysis log
        use_determinism_facts
        (basename filename)
        (concat filename "ER_actionlog")
        (concat filename "deterministic")
        (fun suffix -> concat filename suffix)
    else
      let base = Filename.chop_suffix (Filename.basename filename) ".log" in
        calculate_and_write_analysis log use_determinism_facts base filename
          (base ^ ".deterministic")
          (fun suffix -> base ^ suffix)


