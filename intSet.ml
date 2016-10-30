include BatSet.Make(BatInt)
let pp ?sep = Fmt.iter ?sep iter Fmt.int
