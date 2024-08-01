(* text statistics *)

type stats = Stats of int * int * int * int * int array

(* Utility functions *)

let lines (Stats (l, _, _, _,_)) = l
let sentences (Stats (_, s, _, _,_)) = s
let words (Stats (_, _, w, _,_)) = w
let characters (Stats (_, _, _, c,_)) = c
let histogram (Stats (_, _, _, _, h)) = h

let stats_from_ch ch =
  let histogram = Array.make 256 0 in 
  let ls = ref 0 in 
  let ss = ref 0 in 
  let ws = ref 0 in
  let cs = ref 0 in
  try
    while true do
      begin
        let line = input_line ch in 
        ls := !ls + 1;
        cs := !cs + String.length line;
        String.iter (fun x -> 
          match x with
          '.' | '!' | '?' -> ss := !ss + 1
          |' ' -> ws := !ws + 1
          | _ -> ()) 
          line;
        String.iter (fun c -> 
          let i = int_of_char c in
          histogram.(i) <- histogram.(i) +1)
          line
      end
    done;
    Stats (0, 0, 0, 0, [||])
  with End_of_file -> Stats (!ls, !ss, !ws, !cs, histogram)

let histogram_print arr =
  for i = 0 to (Array.length arr -1) do
    begin
      if (arr.(i) > 0) then (print_char (char_of_int i); print_string " occurs "; print_int (arr.(i)); print_string " times.\n")
    end
  done

let stats_from_file fname =
  let ch = open_in fname in
    let result = stats_from_ch ch in
      close_in ch;
      result
