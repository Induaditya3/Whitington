(* functional *)
let rec ch_statistics ch =
  try
    let _ = input_line ch in 
    ch_statistics ch + 1
  with 
    End_of_file -> 0;;

let f_statistics fname =
  let ch = open_in fname in
  ch_statistics ch;;

(* imperative *)
let channel_statistics ch =
  let lines = ref 0 in 
  let sentences = ref 0 in 
  let words = ref 0 in 
  let chars = ref 0 in 
  try
    while true do 
      let line = input_line ch in
      lines := !lines + 1; 
      chars := !chars + String.length line;
      String.iter 
        (fun c ->
          match c with
          '.' | '?' | '!' -> sentences := !sentences + 1
          | ' ' -> words := !words + 1
          | _ -> ())
        line
    done
  with
    End_of_file ->
      print_string "There are ";
      print_int !lines;
      print_string " lines in the given text consisting of ";
      print_int !sentences;
      print_string " sentences and ";
      print_int !words;
      print_string " words, comparising total of ";
      print_int !chars;
      print_string " characters.";;

let file_statistics fname =
  try
    let ch = open_in fname in 
    channel_statistics ch;
  with 
    Failure _ -> print_string "Error occured.";;
    