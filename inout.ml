(* 4. Write a function which, given a number x, prints the x-times table to a given file name. For example,
table "table.txt" 5 should produce a file table.txt containing the following:
1     2       3       4       5
2     4       6       8       10
3     6       9       12      15
4     8       12      16      20
5     10      15      20      25
Adding the special tabulation character '\t' after each number will line up the columns.*)

(* Some helper functions like map and iter *)

let rec map f l =
  match l with 
  [] -> []
  |[x] -> [f x]
  |h::t -> f h::map f t;;

let rec iter f l =
  match l with
  [] -> ()
  |[x] -> f x
  |h::t -> f h; iter f t;;

(* Core functions *)
let rec numlist n =
  match n with
  0 -> []
  |_ -> numlist (n-1) @ [n];;

let oneEntry ch i =
  output_string ch (string_of_int i);
  output_char ch '\t';;

let oneRow ch n x =
  iter (oneEntry ch) (map (( * ) x) (numlist n));
  output_char ch '\n';;

let write_table_channel ch n =
  iter (oneRow ch n) (numlist n);;

let table fname n =
  let ch = open_out fname in 
  write_table_channel ch n;
  close_out;;

(* Same thing but succinct as well as cryptic *)
let write_table_channel2 ch n =
  iter
  (fun x -> 
    iter
    (fun i ->
      output_string ch (string_of_int i) ; output_char ch '\t'
    )
    (map (( * ) x) (numlist n));
    output_char ch '\n'
  )
  (numlist n);;
