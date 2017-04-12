(** Find which pages have local SSL URLs. *)

(** This is a top-level driver.

  Usage: ownSSL directoris.

  The assumption is that each directory has the following
  structure: It contains a file [index.html].
  This program will extract all URLs in index.html, together
  with information on how they are used, that have an SSL
  scheme and the host equal to the directory name
  (i.e., they start with https://directory/...).
  This is used to figure out pages that need special handling. *)

let current_base = ref "???"

let is_local_uri host url =
  BatOption.default !current_base (Uri.host url) = host

let get_attr key args = BatList.assoc ("", key) args

let add_resource host args urls element =
  try
    let uri = Uri.of_string (get_attr "src" args) in
      if is_local_uri host uri then
        ("Resource for " ^ element, uri) :: urls
      else urls
  with Not_found -> urls

let add_link host args urls element =
  try
    let uri = Uri.of_string (get_attr "href" args) in
      match get_attr "rel" args with
        | exception Not_found -> ("Regular " ^ element, uri) :: urls
        | "dns-prefetch" -> ("DNS prefetch", uri) :: urls
        | "icon" -> ("Icon", uri) :: urls
        | "pingback" -> ("Pingback", uri) :: urls
        | "preconnect" -> ("Pingback", uri) :: urls
        | "prefetch" -> ("Prefetch", uri) :: urls
        | "preload" -> ("Preload", uri) :: urls
        | "prerender" -> ("Prerender", uri) :: urls
        | "stylesheet" -> ("Stylesheet", uri) :: urls
        | _ -> urls
  with Not_found -> urls

let collect_urls_from_attributes host args urls (ns, tag) =
  if ns <> "" then urls else begin match tag with
    | "link" -> add_link host args urls "link"
    | "base" -> begin try
        current_base :=
        BatOption.get @@
        Uri.host @@
        Uri.of_string @@
        get_attr "href" args
      with Not_found -> ()
      end; urls
    | "a" -> add_link host args urls "a"
    | "area" -> add_link host args urls "area"
    | "source" -> add_resource host args urls "source"
    | "img" -> add_resource host args urls "img"
    | "iframe" -> add_resource host args urls "iframe"
    | "embed" -> add_resource host args urls "embed"
    | "video" -> add_resource host args urls "video"
    | "audio" -> add_resource host args urls "audio"
    | "track" -> add_resource host args urls "track"
    | "input" ->
        if List.mem (("", "type"), "image") args then
          add_resource host args urls "image button"
        else urls
    | "script" -> add_resource host args urls "script"
    | "object" -> raise Exit
    | _ -> urls
  end

let collect_urls base urls: Markup.signal -> 'a list = function
  | `Start_element (name, args) ->
      collect_urls_from_attributes base args urls name
  | _ -> urls

let output_error base err =
  Format.printf "@[<v>Could not handle %s: %s@,@]"
    base (Printexc.to_string err)

let output_urls base urls =
  let open Fmt in
  let pp_url_data pp (label, url) =
    pf pp "%s: %a" label Uri.pp_hum url in
    Format.printf "@[<v>@[<v2>URLs for %s:@,%a@]@,@]" base
      (list ~sep:cut pp_url_data) urls

let process dir =
  let open Markup in
  let (input, close_file) = file (Filename.concat dir "index.html")
  and base = Filename.basename dir in try
    current_base := base;
    let document = parse_html input in
    let urls =
      document
        |> signals
        |> fold (collect_urls base) []
    in close_file ();
       output_urls base urls
  with e -> output_error base e

let () = Arg.parse [] process "ownSSL dirs"
