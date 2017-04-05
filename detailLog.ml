let log_channel = ref None
let open_log channel =
  let pp = Format.formatter_of_out_channel channel in
    Format.pp_open_vbox pp 0;
    log_channel := Some (channel, pp)

let close_log () = match !log_channel with
  | Some (chan, pp) ->
      Format.pp_close_box pp ();
      Format.pp_print_flush pp ();
      close_out chan
  | None -> ()
let log f =
  match !log_channel with
    | Some (_, pp) -> f (Format.fprintf pp)
    | _ -> f (Format.ifprintf Format.std_formatter)
