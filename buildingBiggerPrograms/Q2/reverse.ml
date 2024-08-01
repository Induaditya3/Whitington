let rec line_list ch =
  try
    let line = input_line ch in 
    (line_list ch) @ [line]
  with End_of_file -> []

let read_file fname =
  let ch = open_in fname in 
    let list = line_list ch in 
      close_in ch;
      list

let rec copy_lineInner ch l =
  match l with
  [] -> ()
  |[x] -> output_string ch x
  |h::t -> output_string ch h; output_char ch '\n'; copy_lineInner ch t

let copy_line l fname =
  let ch = open_out fname in 
  copy_lineInner ch l;
  close_out ch
