let longest_common_subsequence equal merge l1 l2 =
  let d = Array.make_matrix (1 + List.length l1) (1 + List.length l2) 0
  and final_sequence = ref []
  in
    for i1 = 0 to List.length l1 - 1 do
      for i2 = 0 to List.length l2 - 1 do
        let x1 = BatList.nth l1 i1
        and x2 = BatList.nth l2 i2 in
        let k = equal x1 x2 in
        let dskip1 = d.(i1+1).(i2)
        and dskip2 = d.(i1).(i2+1)
        and dboth = d.(i1).(i2) + if k then 1 else 0 in
          if dboth > dskip1 && dboth > dskip2 then begin
            if k then
              final_sequence := merge x1 x2 :: !final_sequence;
            d.(i1+1).(i2+1) <- dboth
          end else if dskip1 <= dskip2 then
            d.(i1+1).(i2+1) <- dskip2
          else
            d.(i1+1).(i2+1) <- dskip1
      done
    done;
    BatList.rev !final_sequence
