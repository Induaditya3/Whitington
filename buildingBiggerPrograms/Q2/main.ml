try
  (match Sys.argv with
  [|_; inputf; outputf|] ->
    let list = Reverse.read_file inputf in 
    Reverse.copy_line list outputf
  |_ -> print_string "Usage: reversel <given filename> <new filename>\n")
  with e ->
    print_string "An error occured: "; print_string (Printexc.to_string e); exit 1