let print_histogram arr =
  print_string "Character frequencies are :";
  print_newline ();
  for i = 0 to 255 do 
    begin
      if arr.(i) > 0 then 
        (
        print_string "For character \""; print_char (char_of_int i); print_string "\" character count is "; print_int arr.(i); print_newline ()
        )
    end
  done;;

let channel_statistics ch =
  let histogram = Array.make 256 0 in
  try
    while true do
      let line = input_line ch in 
      String.iter 
      (fun c ->
        histogram.(int_of_char c) <- histogram.(int_of_char c) + 1)
      line
    done
  with 
    End_of_file -> print_histogram histogram;;

let file_statistics fname = 
  let ch = open_in fname in 
  channel_statistics ch; close_in ch;;