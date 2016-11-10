let () =
  let log = ref false in
  Arg.parse [
    ("-G", Arg.Set OrderGraph.guid_heuristic, "GUID heuristic (HACK)");
    ("-L", Arg.Set log, "log file")
  ] (JsdeferCommon.analyze log) ""
