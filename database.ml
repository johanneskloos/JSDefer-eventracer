open Sqlite3

exception DBError of Rc.t

let data_bool b = Data.INT Int64.(if b then one else zero)
let check = function
  | Rc.OK
  | Rc.DONE -> ()
  | e -> raise (DBError e)

let run stmt bindings =
  List.iteri (fun i v -> check @@ bind stmt (i+1) v)
    bindings;
  let rec repeat n =
    if n = 0 then raise (DBError Rc.BUSY)
    else match step stmt with
      | Rc.OK
      | Rc.DONE -> ()
      | Rc.BUSY
      | Rc.LOCKED
      | Rc.NOMEM
      | Rc.INTERRUPT ->
          Unix.sleep 1;
          repeat (n-1)
      | e -> raise (DBError e)
  in repeat 3;
     check @@ reset stmt

let write_one insert_scripts insert_nondet insert_race name =
  let open Summary in
  let open Deferability in
    fun script { Summary.script_provenance; script_verdict;
                 has_dom_writes; has_potential_nondeterminism;
                 assumed_deterministic; potential_races } ->
      let (mode, source) = match script_provenance with
        | ScriptInline -> ("inline", "")
        | ScriptSynchronous s -> ("sync", s)
        | ScriptAsynchronous s -> ("async", s)
        | ScriptDeferred s -> ("defer", s)
        | ScriptOther s -> ("other", s)
      and verdict = match script_verdict with
          Deferable -> "deferable"
        | Deferred -> "deferred"
        | HasDOMAccess -> "dom"
        | IsInlineScript -> "inline"
        | IsAsyncScript -> "async"
        | DominatedByDOMAccess -> "<dom"
        | DominatedByInlineScript -> "<inline"
        | DominatedByAsyncScript -> "<async"
        | Nondeterministic -> "nondet"
        | Racing -> "racy"
      and name' = Data.TEXT name
      and script' = Data.INT (Int64.of_int script)
      in run insert_scripts
           [ name'; script';
             Data.TEXT mode;
             Data.TEXT source;
             Data.TEXT verdict;
             data_bool has_dom_writes;
             data_bool assumed_deterministic];
         StringSet.iter
           (fun nondet -> run insert_nondet [name'; script'; Data.TEXT nondet])
           has_potential_nondeterminism;
         Races.RaceSet.iter
           (fun { Races.racing_ev; Races.refs } ->
              run insert_race
                [name'; script';
                 Data.INT (Int64.of_int racing_ev);
                 Data.TEXT (Fmt.to_to_string Trace.pp_reference refs)])
           potential_races


let write_to_database filename { Summary.per_script; name } =
  let db = db_open ~mode:`NO_CREATE ~mutex:`FULL ~cache:`SHARED filename
  in try
    run (prepare db "BEGIN TRANSACTION") [];
    let ins_scripts =
      prepare db
        ("INSERT INTO scripts (name, script, mode, source, " ^
         "verdict, dom_writes, assumed_deterministic) " ^
         "VALUES (?, ?, ?, ?, ?, ?, ?)")
    and ins_nondet =
      prepare db
        "INSERT INTO nondeterminism (name, script, nondet) VALUES (?, ?, ?)"
    and ins_race =
      prepare db
        "INSERT INTO races (name, script, race, ref) VALUES (?, ?, ?, ?)"
    in IntMap.iter (write_one ins_scripts ins_nondet ins_race name) per_script;
       run (prepare db "COMMIT") []
  with DBError e ->
    Log.err (fun m -> m "Database error: %s" (errmsg db));
    failwith "Database error"

