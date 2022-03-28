let source =
  Scanf.sscanf Sys.ocaml_version "%u." (Printf.sprintf "compat.ml.%d")

let () =
  let ic = open_in source in
  try while true do
    print_endline (input_line ic)
  done with End_of_file -> close_in ic
