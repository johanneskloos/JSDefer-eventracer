let l1 = ["a"; "n"; "a"; "n"; "a"; "s"]
let l2 = ["b"; "a"; "n"; "a"; "n"; "e"; "n"; "m"; "u"; "s"]

let lcs =
  LongestCommonSubsequence.longest_common_subsequence
    (=) (fun _ x -> x) l1 l2

let () =
  let open Fmt in
    pr "%a" (list ~sep:nop string) lcs
