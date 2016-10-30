include BatMap.Make(BatInt)
let pp ?sep fmt = Fmt.iter_bindings ?sep iter fmt
let pp_default ?esep ?psep fmt =
  pp ?sep:esep (Fmt.pair ?sep:psep Fmt.int fmt)
