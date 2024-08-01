try
  match Sys.argv with
  [|_ ;phrase; fname|] ->
    Search.read_search phrase fname; print_newline ()
  |_ -> print_string "Usage: search <phrase> <filename>"
  with e ->
    print_string "Error occured"; prerr_string (Printexc.to_string e); print_newline ()