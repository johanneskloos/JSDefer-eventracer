let re_dot = Str.regexp_string "."
let totally_an_alias parts1 parts2 =
  let open BatList in
  let len = Pervasives.min (length parts1) (length parts2) in
  let l1 = drop (length parts1 - len) parts1
  and l2 = drop (length parts2 - len) parts2
  in l1 = l2 || (len > 2 && tl l1 = tl l2)

let canonical_uri base url =
  let open Uri in
  let url = of_string url in
  let url = match host url with
    | Some host ->
        (* GROSS HACK *)
        let parts_host = Str.split re_dot host
        and parts_base = Str.split re_dot base
        in if totally_an_alias parts_host parts_base then
          with_host url (Some base)
        else url
    | None ->
        with_host url (Some base)
  in with_scheme url (Some "http") |> canonicalize

let read_tasks base =
  let csv = Csv.load (Filename.concat base "result.csv") in
    BatList.filter_map
      (function
         | [_; id; "sync"; _; verdict; _; _; _; _; url ] ->
             Some (int_of_string id, (url, verdict = "deferable"))
         | _ -> None)
      csv
    |> BatList.sort (fun (id1, _) (id2, _) -> compare id1 id2)
    |> BatList.map (fun (_, (url, defer)) -> (canonical_uri base url, defer))

let extract_sync_scripts base scripts =
  let open Soup in
    scripts
    |> filter (fun node -> has_attribute "src" node &&
                           not (has_attribute "async" node) &&
                           not (has_attribute "defer" node))
    |> fold (fun scripts node -> (R.attribute "src" node, node) :: scripts) []
    |> BatList.rev_map (fun (url, node) -> (canonical_uri base url, node))

let merge_tasks l1 l2 =
  let open Fmt in
    pr "@[<v>URIs in tasks:@,%a@,@,URIs in scripts:@,%a@,@]"
      (list ~sep:cut (using fst Uri.pp_hum)) l1
      (list ~sep:cut (using fst Uri.pp_hum)) l2;
    LongestCommonSubsequence.longest_common_subsequence
      (fun (url1, _) (url2, _) -> Uri.equal url1 url2)
      (fun (url, defer) (_, node) -> (url, defer, node))
      l1 l2

let read_document base =
  let open Soup in
  let document = read_file (Filename.concat base "index.html") |> parse
  in let scripts = document $$ "script" |> extract_sync_scripts base
  in (scripts, document)

let process base =
  let tasks = read_tasks base
  and (scripts, document) = read_document base
  in let shared_scripts = merge_tasks tasks scripts
  in 
    Soup.to_string document
    |> Soup.write_file (Filename.concat base "index.nodefer.html");
    BatList.iter (function
                    | (_, true, node) -> Soup.set_attribute "defer" "" node
                    | _ -> ())
      shared_scripts;
    let outfile = Filename.concat base "index.defer.html" in
      Soup.to_string document |> Soup.write_file outfile

let () = Arg.parse [] process "rewrite dirs"
