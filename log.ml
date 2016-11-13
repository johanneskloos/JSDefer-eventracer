let source = ref Logs.default
let set_source_for_file filename =
  source := Logs.Src.create filename
