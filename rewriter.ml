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
    |> BatList.map (fun (_, (url, defer)) -> (canonical_uri url, defer))

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

let merge_tasks =
  LongestCommonSubsequence.longest_common_subsequence
    (fun (url1, _) (url2, _) -> Uri.equal url1 url2)
    (fun (url, defer) (_, node) -> (url, defer, node))

let read_document base =
  let open Soup in
  let document = read_file (Filename.concat base "index.html") |> parse
  in let scripts = begin document $$ "script"
    |> extract_sync_scripts (Uri.of_string (Filename.basename base))
    end
  in (scripts, document)

let process base =
  let tasks = read_tasks base
  and (scripts, document) = read_document base
  in let shared_scripts = merge_tasks tasks scripts
  in BatList.iter (function
                     | (_, true, node) -> Soup.set_attribute "defer" "" node
                     | _ -> ())
       shared_scripts;
     let outfile = Filename.concat base "index.defer.html" in
     Soup.to_string document |> Soup.write_file outfile

let () = Arg.parse [] process "rewrite dirs"
