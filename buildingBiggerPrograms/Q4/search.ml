let rec contain search n text =
  if (String.equal search (String.sub text 0 n)) then true
  else
    begin
      if (String.length text = n) then false
      else contain search n (String.sub text 1 (String.length text-1))
    end


let rec list_lines ch =
  try
    let line = input_line ch in 
    line::list_lines ch
  with End_of_file -> []

let read_search s fname =
  let ch = open_in fname in
  let list = list_lines ch in 
  List.iter (fun x -> if contain s (String.length s) x then print_string x ) list;
  close_in ch
