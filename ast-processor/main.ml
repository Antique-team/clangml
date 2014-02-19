open Hello_ast

let () =
  match Sys.argv with
  | [|_; "expr"|] ->
      let value : expr = Marshal.from_channel stdin in
      ()

  | [|_; "stmt"|] ->
      let value : stmt = Marshal.from_channel stdin in
      ()

  | [|_; "decl"|] ->
      let value : decl = Marshal.from_channel stdin in
      C_utils.ppi_c_prog "" stdout (Transform.c_prog_from_decl value)

  | _ ->
      failwith ("Usage: " ^ Sys.argv.(0) ^ " <expr|stmt|decl>")
