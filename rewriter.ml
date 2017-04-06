let re_tab = Str.regexp "\t"

let parse_script_list filename =
  BatFile.lines_of filename
    |> BatEnum.map (fun line -> match Str.bounded_split re_tab line 2 with
                      | [num; url] -> (int_of_string num, url)
                      | _ -> failwith ("Can't parse line " ^ line))
    |> BatList.of_enum
    |> BatList.sort (fun (ord1, _) (ord2, _) -> compare ord1 ord2)
    |> BatList.map snd

let transform_html scripts document =
  let open Soup in
  let todo_list = ref scripts 
  and script_elements = document $$ "script"
  in iter (fun elt ->
             match !todo_list with
               | wanted :: rest ->
                   if attribute "src" elt = Some wanted then begin
                     todo_list := rest;
                     set_attribute "defer" "" elt
                   end
               | [] -> ())
       script_elements;
  if !todo_list <> [] then failwith "Couldn't defer all scripts"
                           
let defer_directory dir =
  let open Filename in
  let open Soup in
  let scripts = parse_script_list (concat dir "defer")
  and document = read_file (concat dir "index.html") |> parse
  in transform_html scripts document;
     to_string document |> write_file (concat dir "index.defer.html")
