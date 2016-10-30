open Trace

type value_lattice =
  | Anything
  | AnyValue
  | SpecificValue of value

let pp_value_lattice pp =
  let open Fmt in function
    | Anything -> if utf_8 pp then string pp "⊤" else string pp "(anything)"
    | AnyValue -> string pp "?"
    | SpecificValue i -> pp_value pp i

let per_event_reads commands =
  BatList.fold_right
    (fun op reads -> match op with
       | Read (Runknown, _) ->
	 reads (* Can't say anything here *)
       | Read (ref, Vunknown) ->
	 if not (ReferenceMap.mem ref reads) then
	   ReferenceMap.add ref None reads
	 else
	   reads
       | Read (ref, value) ->
           ReferenceMap.modify_def None ref
             (function
                | None -> Some value
                | Some value' when value = value' ->
                    Some value
                | Some value' ->
                    Format.eprintf "Non-deterministic reading of %a: %a vs. %a@."
                      pp_reference ref pp_value value pp_value value';
                    None)
             reads
       | Write (Runknown, _) ->
	 failwith "Read/write to unknown location"
       | Write (ref, _) ->
	 ReferenceMap.remove ref reads
       | Post _ | Enter _ | Exit -> reads)
    commands ReferenceMap.empty

let per_event_writes_posts commands =
  BatList.fold_left
    (fun (post, posts) op -> match op with
       | Write (Runknown, _) ->
	 failwith "Read/write to unknown location"
       | Write (ref, Vunknown) ->
	 (ReferenceMap.add ref None post, posts)
       | Write (ref, value) ->
	 (ReferenceMap.add ref (Some value) post, posts)
       | Post id -> (post, IntSet.add id posts)
       | Read _ | Enter _ | Exit -> (post, posts))
    (ReferenceMap.empty, IntSet.empty) commands

type event_standalone_spec = {
  reads: value option ReferenceMap.t;
  writes: value option ReferenceMap.t;
  posts: IntSet.t
}

let per_event_specification specs { id; commands } =
  let reads = per_event_reads commands
  and (writes, posts) = per_event_writes_posts commands
  in IntMap.add id { reads; writes; posts } specs

let per_event_specification { events } =
  List.fold_left per_event_specification IntMap.empty events

