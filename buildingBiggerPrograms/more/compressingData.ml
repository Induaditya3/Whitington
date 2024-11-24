(* 1. How much complexity did using the input and output types add to compress and decompress
in our byte-by-byte example? Rewrite the functions so they just operate over lists of integers, in
functional style, and compare the two. *)

(* my version - answer to first question - certainly more than using default data types in the sense that code gets longer but building abstractions makes code more
 extensible and maintainable  *)
 let rec take l n =
  match l with 
  [] -> if n = 0 then [] else raise (Failure "take")
  |h::t -> if n = 0 then [] else h::take t (n-1)

let rec drop l n = 
  match l with 
  [] -> if n = 0 then [] else raise (Failure "drop")
  |h::t -> if n = 0 then l else drop t (n-1)

let rec copy e n =
  if n = 1 then [e] else e::copy e (n-1)

let rec decompress l =
  match l with
  [a] -> []
  |[] -> raise (Invalid_argument "decompress")
  |h::t -> 
    if (h >= 0 && h < 128 ) then 
      begin
        let n = h + 1 in 
        (take t n)@decompress (drop t n)
      end
    else if (h > 128 && h < 256) then
      begin
        let n = 257 - h in 
        (copy (List.hd t) n)@ decompress (List.tl t)
      end
    else
      []

let get_same l =
  let rec get_count lst e c =
    if c = 128 then 128 else 
      try
        match lst with
        [] -> c
        | h::t -> if h = e then get_count t e (c+1) else c
      with _ -> c
    in 
  get_count l (List.hd l) 0

let get_different l =
  let rec getdiffinner lst a n =
    if n = 128 then List.rev a else
      try
        match lst with
        [] -> List.rev a
        |[h'] -> if List.length a = 0 then [h'] else
          if h' <> List.hd a then List.rev (h'::a) else List.rev (drop a 1)
        |h::t -> if h <> List.hd t then getdiffinner t (h::a) (n+1) else List.rev a
      with _ -> List.rev a
    in
  getdiffinner l [] 0

let compress l =
  let rec compress_inner lst a =
    try
      let same = get_same lst in 
      if same <> 1 then 
        compress_inner (drop lst same) ((List.hd lst)::(257-same)::a)
      else
        let differ = get_different lst in 
        let differ_len = List.length differ in
        compress_inner (drop lst differ_len) ((List.rev differ) @ (differ_len-1)::a)
    with _ -> List.rev (128::a) in 
    compress_inner l []

(* author's version *)
type run = Same of int * int | Diff of int list

let rec get_same x n l =
  match l with 
  h::t when h = x -> get_same x (n+1) t
  | _ -> (n,l)

let rec get_different a l =
  match l with 
  [] -> (List.rev a, [])
  |h::t -> 
    if a = [] then get_different [h] t
    else if h <> List.hd a then get_different (h::a) t
    else (List.rev (List.tl a), List.hd a::l)

let getrun l =
  match l with
  [] -> raise (Invalid_argument "getrun")
  |h::_ ->
    match get_same h 0 l with
    1, _ -> 
      let diff, rest = get_different [] l in (Diff diff, rest)
    |same, rest -> 
      (Same (same, h), rest)

let chars_of_runs r =
  match r with 
  Same (length , h) -> (257-length)::[h]
  |Diff chars -> List.length chars -1:: chars

let rec compress_inner a l =
  match l with 
  [] -> List.concat (List.map chars_of_runs (List.rev a))
  | _ -> let run, rest = getrun l in compress_inner (run::a) rest

let compress l = compress_inner [] l

let rec decompress_inner a l =
  match l with 
  [128] -> List.concat (List.rev a)
  |[] | [_] -> raise (Invalid_argument "decompress_inner")
  |h::t::t' -> 
    if h < 128 then
      decompress_inner ((take (t::t') (h+1))::a) (drop (t::t') (h+1))
    else if h > 128 then
      decompress_inner ((Array.to_list (Array.make (257-h) t))::a) t'
    else decompress_inner a []
  
let decompress l = decompress_inner [] l

(* 2. Replace our manual tree of codes with a tree automatically generated from the lists of codes used
for compression. The tree will have no data at its branches (since no code is a prefix of another), and
will have data at only some of its leaves. Define a suitable data type first. *)
(* my version - here existence of left branch means 0 there and right means 1 *)
type int_opt = Some of int | None
type tree = Br of int_opt * tree * tree | Lf

let rec add_code tr l i =
  match tr with
  Lf ->
    if List.length l = 0 then Br (Some i, Lf, Lf)
    else
      begin
        if List.hd l = 0 then Br (None, add_code Lf (List.tl l) i, Lf)
        else Br (None, Lf, add_code Lf (List.tl l) i)        
      end
  | Br (a, lf, rt) -> 
        if List.hd l = 0 then Br (a, add_code lf (List.tl l) i, rt)
        else Br (a, lf, add_code rt (List.tl l) i)

let tree_of_codes ar =
  let tr = ref Lf in 
  for i = 0 to Array.length ar -1 do
    tr := add_code !tr (ar.(i)) i
  done;
  !tr

(* author's version *)
type tree = Lf | Code of int | Br of tree * tree
let rec add_elt tr (l, n) =  (* elt stands for Extract, Load, and Transform *)
  match l with
  0::m ->
    begin match tr with
      Lf -> Br (add_elt Lf (m, n), Lf)
      |Br (left, right) -> Br (add_elt left (m,n), right)
      |Code _ -> raise (Failure "collision")
    end
  |1::m ->
    begin match tr with
      Lf -> Br (Lf, add_elt tr (m,n))
      |Br (left, right) -> Br (left, add_elt right (m,n))
      |Code _ -> raise (Failure "collision")
    end
   
  |_ -> raise (Failure "bad code")

let make_tree ar numbers =
  List.fold_left 
  add_elt
  Lf
  (List.combine (Array.to_list ar) numbers)

(* 4. Write a function which, given input data, will calculate a histogram of the frequencies of different
runs of white and black. This could be used to build custom codes for each image, improving
compression. *)
(* my version - incorrect - debugging or complete rewrite is required*)
type input =
{pos_in : unit -> int;
seek_in : int -> unit;
input_char : unit -> char;
in_channel_length : int}

type input_bits =
{input : input;
mutable byte: int;
mutable bit : int}

type output =
{output_char : char -> unit;
out_channel_length : unit -> int}

type output_bits =
{output :output;
mutable obyte : int;
mutable obit : int}

let output_of_buffer b =
  {output_char = Buffer.add_char b;
  out_channel_length = fun () -> Buffer.length b}

let output_bits_of_output o =
  {output = o;
  obyte = 0;
  obit = 7}

let rec putbit o b =
  if o.obit = (-1) then
    (o.output.output_char (char_of_int o.obyte);
    o.obit <- 7;
    o.obyte <- 0;
    putbit o b)
  else
    (if b then
      o.obyte <- o.obyte lor (1 lsl o.obit));
    o.obit <- o.obit - 1

let packedstring s =
  let buff = (Buffer.create (String.length s / 8)) in
  let o = output_bits_of_output (output_of_buffer buff) in 
  for x = 0 to String.length s - 1 do
    putbit o (s.[x] = '1')
  done;
  o.output.output_char (char_of_int o.obyte);
  Buffer.contents buff

let input_of_string s =
  let pos = ref 0 in
  {pos_in = (fun () -> !pos);
  seek_in = (fun x ->
    if x < 0 then raise (Invalid_argument "seek before beginning")
    else pos := x);
  input_char = (fun () ->
    if !pos < String.length s then (let c = s.[!pos] in pos := !pos + 1; c) else raise End_of_file);
  in_channel_length = String.length s}

let input_bits_of_input i =
  {input = i;
  byte = 0;
  bit = 128}

let rec getbit i =
  if i.bit = 0 then
    (i.bit <- 128;
    i.byte <- int_of_char (i.input.input_char ());
    getbit i)
  else
    let bit = i.byte land i.bit > 0 in 
    i.bit <- i.bit / 2;
    bit

let rewind i = i.seek_in (i.pos_in () - 1)

let peekbit i =
  if i.bit = 0 then 
    let byte = int_of_char(i.input.input_char ()) in
    byte land 128 > 0
  else
    i.byte land i.bit > 0

let rec read_up_to v i n =
  if v <> peekbit i then (n, v)
  else
    (ignore (getbit i);
    read_up_to v i (n+1))

let arr_of_white_code = Array.make 1791 0
let arr_of_black_code = Array.make 1791 0

let histogram_white_black s =
  let i = input_bits_of_input (input_of_string (packedstring s)) in 
  try
    while true do
      let n, iswhite = read_up_to (peekbit i) i 0 in 
      if iswhite then 
        arr_of_white_code.(n) <- arr_of_white_code.(n) +1
      else
        arr_of_black_code.(n) <- arr_of_black_code.(n) + 1
    done;
  with End_of_file -> ()

(* testing *)
let input_data =
  "00000000000000000000000000000000000000000000000000000000000000000000000000000000\
  00000000000000000000000000000000000000000000000000000000000000000000000001000000\
  00000000111111110000000000011111111100000000000000000000000000000000000111100000\
  00000011000000011100000001110000001110000000000000000000000000000000000011000000\
  00000110000000001110000011000000000110000000000000000000000000000000000011000000\
  00001110000000000111000111000000000000000000000000000000000000000000000011000000\
  00001100000000000111000110000000000000000000000000000000000000000000000011000000\
  00001100000000000011001110000000000000000011100000000100111000011100000011000000\
  00011100000000000011001110000000000000001111111000111111111101111110000001000000\
  00011100000000000011101100000000000000001000011000001110001111000111000001000000\
  00011100000000000011101100000000000000000000011000001100000110000011000001000000\
  00011100000000000011001110000000000000000000011000001100000110000011000001000000\
  00001100000000000011001110000000000000000111011000001100000110000011000001000000\
  00001110000000000011000110000000000000011100011000001100000110000011000001000000\
  00001110000000000110000111000000000000011000011000001100000110000011000011000000\
  00000111000000000110000011100000000000011000011000001100000110000011000011100000\
  00000011100000001100000001110000000010010001110000011000001100000110000111000000\
  00000011111111100000000001111111111000111110111000111100011110000111001111100000\
  00000000011100000000000000001111000000001000000000000000000000000000000000000000\
  00000000000000000000000000000000000000000000000000000000000000000000000000000000\
  00000000000000000000000000000000000000000000000000000000000000000000000000000000";;

histogram_white_black input_data;

for x = 0 to Array.length arr_of_black_code -1 do 
   if arr_of_black_code.(x) > 0 then 
    Printf.printf "Number of %i length black runs = %i \n" x arr_of_black_code.(x)
  done;

for x = 0 to Array.length arr_of_white_code -1 do 
  if arr_of_white_code.(x) > 0 then 
    Printf.printf "Number of %i length white runs = %i \n" x arr_of_white_code.(x)
  done
