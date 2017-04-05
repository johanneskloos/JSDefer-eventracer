type race = {
  script: int;
  script_ev: int;
  racing_ev: int;
  refs: Trace.reference
} [@@deriving ord]

let pp_race pp { script; script_ev; racing_ev; refs } =
  let open Fmt in
    pf pp "@[<h>%d:%d - %d on %a@]" script script_ev racing_ev
      Trace.pp_reference refs
let show_race = Fmt.to_to_string pp_race

module Race = struct
  type t = race
  let compare = compare_race
end
module RaceSet = BatSet.Make(Race)

let pp_races =
  let open Fmt in
    iter ~sep:(suffix sp (const string ";")) RaceSet.iter pp_race

let find_potential_races races script_set =
  List.fold_left (fun pr { Trace.ev1; ev2; var } ->
                    match List.mem ev1 script_set, List.mem ev2 script_set,
                          script_set with
                      | true, false, script::_ ->
                          RaceSet.add { script; script_ev = ev1; racing_ev = ev2;
                                        refs = var } pr
                      | false, true, script::_ ->
                          RaceSet.add { script; script_ev = ev2; racing_ev = ev1;
                                        refs = var } pr
                      | _, _, _ -> pr)
    RaceSet.empty races


