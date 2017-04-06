type task = { url: Uri.t; defer: bool }

let canonical_uri url =
  let open Uri in
    with_scheme (of_string url) (Some "http") |> canonicalize

let read_tasks base =
  let csv = Csv.load (Filename.concat base "result.csv") in
    BatList.filter_map
      (function
         | [_; id; "sync"; _; verdict; _; _; _; _; url ] ->
             Some (int_of_string id, (url, verdict = "deferable"))
         | _ -> None)
      csv
    |> BatList.sort (fun (id1, _) (id2, _) -> compare id1 id2)
    |> BatList.map (fun (_, (url, defer)) -> { url = canonical_uri url; defer })

let resolve_uri base url =
  let open Uri in resolve "http" base (of_string url) |> canonicalize

let extract_sync_scripts base_url scripts =
  let open Soup in
    scripts
    |> filter (fun node -> has_attribute "src" node &&
                           not (has_attribute "async" node) &&
                           not (has_attribute "defer" node))
    |> fold (fun scripts node -> (R.attribute "src" node, node) :: scripts) []
    |> BatList.rev_map (fun (url, node) -> (resolve_uri base_url url, node))

let rec apply_tasks tasks reals =
  match tasks, reals with
    | { url=url1; defer } :: tasks, (url2, node)::reals when url1 = url2 ->
        if defer then Soup.set_attribute "defer" "" node;
        apply_tasks tasks reals
    | _ :: tasks, reals -> apply_tasks tasks reals
    | [], [] -> ()
    | [], scripts ->
        failwith @@
        let open Fmt in
        strf "@[<v>Left-over scripts:@,%a@]"
          (list ~sep:cut (using fst Uri.pp_hum)) scripts

let process base =
  let open Soup in
  let tasks = read_tasks base
  and document = read_file (Filename.concat base "index.html") |> parse in
    begin document $$ "script"
    |> extract_sync_scripts (Uri.of_string (Filename.basename base))
    |> apply_tasks tasks
    end;
    to_string document |> write_file (Filename.concat base "index.deferred.html")

let () = Arg.parse [] process "rewrite directories"
