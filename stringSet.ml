include BatSet.Make(BatString)
let pp ?sep = Fmt.iter ?sep iter Fmt.string
