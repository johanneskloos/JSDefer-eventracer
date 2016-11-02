open Graph

module P(G: Sig.P) = struct
  let rec recrm todo g = match todo with
    | [] -> g
    | v::todo ->
        if not (G.mem_vertex g v) then
          recrm todo g
        else if G.in_degree g v > 0 then
          recrm todo g
        else
          recrm (G.succ g v @ todo) (G.remove_vertex g v)

  let remove_below g v =
    if not (G.mem_vertex g v) then
      g
    else
      recrm (G.succ g v) (G.remove_vertex g v)
end
