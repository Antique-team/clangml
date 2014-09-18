
(* run the given command and return its output as a string *)
let get_command_output cmd =
  let cmd_out = Unix.open_process_in cmd in
  let buff_size = 1024 in
  let buff = Buffer.create buff_size in
  let _ =
    try
      while true do
        Buffer.add_string buff (input_line cmd_out);
        Buffer.add_char buff '\n'; (* stripped by input_line *)
      done;
    with
    | End_of_file -> ignore(Unix.close_process_in cmd_out)
    | exn -> ignore(Unix.close_process_in cmd_out); raise exn
  in
  Buffer.contents buff
