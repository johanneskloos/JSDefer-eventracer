open Trace
open ClassifyTask

module Path = Graph.Path.Check(DependencyGraph)

let rec fold_ord_pairs f a = function
  | x :: l -> BatList.fold_left (fun a y-> f x y a) a l
  | [] -> a

type triples = { posts: IntSet.t IntMap.t; reverse: (int*int*int) list; race: (int*int*int) list }

let build_triple_graph filename =
  let ({ events; deps }, classification) =
    filename |> CleanLog.load |> Trace.parse_trace |> ClassifyTask.classify
  in let post_sets =
    BatList.fold_right
      (fun { id; commands } post_sets ->
         let posts = BatList.filter_map
                       (function
                          | Post id when id <> (-1) ->
                              begin
                                match IntMap.find id classification with
                                  | ImmediateEventHandlerScript
                                  | ShortTimerEventHandlerScript
                                  | LongTimerEventHandlerScript
                                  | AnimationEventHandlerScript
                                  | ResourceLoadEventHandlerScript
                                  | WindowInteractiveScript
                                  | WindowCompleteScript
                                  | UIEventHandlerScript
                                  | OtherEventHandlerScript
                                  | UnclearScript ->
                                      Some (IntSet.add id (IntMap.find id post_sets))
                                  | _ -> None
                                  | exception Not_found -> None
                              end
                          | _ -> None)
                       commands
         in IntMap.add id (List.fold_left IntSet.union IntSet.empty posts) post_sets)
      events IntMap.empty
  and check = Path.create deps
  and toplevel =
    BatList.filter_map (fun { id } ->
                          match IntMap.find id classification with
                            | InlineScript
                            | ExternalSyncScript
                            | ExternalAsyncScript
                            | ExternalDeferScript
                            | ExternalUnknownScript -> Some id
                            | _ -> None
                            | exception Not_found -> None)
      events
  in
    (* Enumerate all triples and classify them *)
    fold_ord_pairs (fun s1 s2 triples ->
                      if Path.check_path check s1 s2 then
                        IntSet.fold
                          (fun t { posts; reverse; race } ->
                             if Path.check_path check t s2 then
                               { posts; reverse; race }
                             else if Path.check_path check s2 t then
                               { posts; reverse = (s1,s2,t) :: reverse; race }
                             else
                               { posts; race = (s1,s2,t) :: race; reverse })
                          (IntMap.find s1 post_sets) triples
                      else triples)
      { posts = IntMap.filter (fun id _ -> BatList.mem id toplevel) post_sets;
        reverse = []; race = [] } toplevel

let pp_triples =
  let open Fmt in
  let comma = const string "," in
    list ~sep:sp (using (fun (a,b,c) -> ((a,b),c))
                    (pair ~sep:comma (pair ~sep:comma int int) int))

let () =
  Arg.parse []
    (fun filename ->
       let { posts; reverse; race } = build_triple_graph filename in
         Format.printf "@[<v>%s:@,\
                        @[<v2>Post sets:@,%a@]@,\
                        @[<hov2>Reverse orderings: %a@]@,\
                        @[<hov2>Races: %a@]@,@]"
           filename
           Fmt.(IntMap.pp_default ~esep:cut ~psep:(const string ": ")
              (box ~indent:2 (IntSet.pp ~sep:sp))) posts
           pp_triples reverse
           pp_triples race)
    ""
