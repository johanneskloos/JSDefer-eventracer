let run filename =
  let log = CleanLog.load filename
  in let trace = Trace.parse_trace log
  in let reduced_trace = TraceReduce.cleanup trace
  in Format.printf "@[<v>%s:@,@,%a@,@]" filename
       Trace.pp_trace_with_deps reduced_trace

let () = Arg.parse [] run "..."
