include BatMap.Make(BatString)
let pp ?sep fmt = Fmt.iter_bindings ?sep iter fmt
let pp_default ?esep ?psep fmt =
  pp ?sep:esep (Fmt.pair ?sep:psep Fmt.string fmt)
