(* 1. Given a list of pairs of integers such as [(1, 2); (5, 6); (6, 6); (7, 5)], write a function
to return a string representation such as "(1, 2) --> (5, 6) --> (6, 6) --> (7, 5) --> (1,
2)". *)

(* my version - in the case of empty list, it results in match failure*)
let rec print_inner b (h::t) = 
  if t = [] then
    begin
      Printf.bprintf b "(%i, %i)" (fst h) (snd h);
      Buffer.contents b
    end
  else
    begin
      Printf.bprintf b "(%i, %i) --> " (fst h) (snd h);
      print_inner b t
    end

let print l =
  let b = print_inner (Buffer.create 100) l in
  let first = List.hd l in 
  b ^ Printf.sprintf " --> (%i, %i)" (fst first) (snd first)

(* author's version - in the case of empty list, it outputs "" *)
let rec cycle_of_points_inner b l =
  match l with 
  [] ->
    Buffer.contents b
  | [(x, y)] ->
    Printf.bprintf b "(%i, %i)" x y;
    Buffer.contents b
  | h::t ->
    Printf.bprintf b "(%i, %i) --> " (fst h) (snd h);
    cycle_of_points_inner b t

let cycle_of_points l =
  match l with
  [] -> ""
  |h::t ->
    cycle_of_points_inner (Buffer.create 256) (h::t@[h])

(* 2. Write a function which, given a string, returns another string which represents the first using
hexadecimal numbers. For example, the input string Hello should yield the output 48656c6c6f
since 'H' has ASCII code 0x48 and so on. *)

(* my version *)
let hex s =
  let b = Buffer.create 100 in 
  for x = 0 to String.length s -1 do
    Printf.bprintf b "%x" (int_of_char s.[x])
  done;
  Buffer.contents b

(* author's version *)
let hex_of_string s =
  let b = Buffer.create (String.length s * 2) in 
  String.iter 
   (fun c -> Printf.bprintf b "%02X" (int_of_char c))
   s;
  Buffer.contents b

(* 3. Why does the following code cause a type error?
# let mkstring () = "string";;
val mkstring : unit -> string = <fun>
# Printf.printf (mkstring ());
;;
Error: This expression has type string but an expression was expected of type
('a, out_channel, unit) format =
('a, out_channel, unit, unit, unit, unit) format6
What can be done to fix it? *)

(* my version - Printf.printf expects format string as its first argument so it throws type error *)
let mkstring () = "string";;
Printf.printf "%s" (mkstring ());;

(* author's version - exactly same *)
(* The format string for Printf.printf must be known at compile time. The solution for printing the result
of mkstring using printf is the %s format specification. *)

(* 4. Use the * syntax described in the Printf module documentation to write a function which can print
a table of integers to a given width. For example, given width 10, we might see:
(        1)
(       28)
(    33241)
(        0)
         *)

(* my version *)
let print_table width t =
  List.iter
   (Printf.printf "(%*i)\n" width)
   t

(* author's version - basically same *)