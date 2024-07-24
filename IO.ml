(* One important note -> In OCaml, file pointers are called channel for some reason *)

(* printing key value pair*)
let printPair (k,v) = 
  print_int k; print_newline (); print_string v; print_newline ();;

(* printing list of key value pair*)
let rec printDict l =
  match l with
  [] -> ()
  |h::t -> printPair h; printDict t;;

(* Sample dict *)
let a = [(1, "one");( 2,"two");(3, "three"); (4, "five");(5,"five")];;

(* similar to map but which returns nothing that is unit () *)
let rec iter f l =
  match l with
  [] -> ()
  |h::t -> f h ; iter f t;;

(* print dict in terms iter *)
let print_dict = iter printPair;;

(* takes input interactively *)
let rec inDict () =
  let k = read_int (); in 
   if k = 0 then [] 
    else let v = read_line (); in
     (k,v)::inDict ();;

(* similar to previous one with exception handling *)
let rec proIn () =
  try 
     let k = read_int () in
      if k = 0 then [] else 
       let v = read_line () in 
        (k,v)::proIn ()
  with
  Failure _ ->
  print_newline ();
  print_string "Enter valid integer";
  print_newline ();
  proIn ();;

(* writes  key value pair to a file given file pointer*)
let entryToChannel ch (k,v) =
  output_string ch (string_of_int k);
  output_char ch '\n';
  output_string ch v;
  output_char ch '\n';;

(* writes dict *)
let dictToChannel ch l =
  iter (entryToChannel ch) l;;

(* writes dict to a file *)
let dictToFile fname d =
  let ch = open_out fname in
    dictToChannel ch d;
    close_out ch;;

(* for retrieving two lines from channel as key value pair *)
let entryFromCh ch =
  let k = input_line ch in
   let v = input_line ch in
    (int_of_string k , v);;

(* reading two lines at time build pair which further will be used to build list of pair aka dict *)
let rec dictFromCh ch =
  try 
     let e = entryFromCh ch in
      e::dictFromCh ch
  with 
    End_of_file -> [];;

(* read from file to form a dict *)
let dicFromF fname =
  let ch = open_in fname in
    let d = dictFromCh ch in
      close_in ch;
        d;;