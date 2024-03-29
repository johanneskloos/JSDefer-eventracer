(** Rewrite pages for deferability. *)

(** This is a top-level driver.

  Usage: rewriter directories

  The assumption is that each directory has the following
  structure: It contains a file [results.csv], detailing the results
  of deferablity analysis, and a file [index.html], containing
  the HTML file to rewrite. It produces two new files,
  [index.nodefer.html] and [index.defer.html]. The former is the
  same as [index.html], up to pretty-printing of HTML, while
  the latter also contains additional [defer] attributes. *)

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
  let l = LongestCommonSubsequence.longest_common_subsequence
      (fun (url1, _) (url2, _) -> Uri.equal url1 url2)
      (fun (url, defer) (_, node) -> (url, defer, node))
      l1 l2
  in pr "Incoming: %d tasks, %d scripts. LCS of %d tasks:@,%a@,"
       (List.length l1) (List.length l2) (List.length l)
       (list ~sep:cut @@
        using (fun (id, defer, _) -> (defer, id)) @@ hbox @@
        pair ~sep:(const string "\t") bool Uri.pp_hum)
       l;
     BatList.filter_map
       (fun (_, defer, node) -> if defer then Some node else None) l

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
    BatList.iter (Soup.set_attribute "defer" "") shared_scripts;
    let outfile = Filename.concat base "index.defer.html" in
      Soup.to_string document |> Soup.write_file outfile

let () = Arg.parse [] process "rewrite dirs"
