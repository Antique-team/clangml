
(* run the given command and return its output *)
let get_command_output (cmd: string): string list =
  let cmd_out = Unix.open_process_in cmd in
  let res = ref [] in
  (try
     while true do
       (* n.b. input_line strips the trailing '\n' *)
       res := (input_line cmd_out) :: !res
     done;
   with
   | End_of_file -> ignore(Unix.close_process_in cmd_out)
   | exn -> ignore(Unix.close_process_in cmd_out); raise exn
  );
  !res
