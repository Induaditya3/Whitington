(* some necessary declarations *)
type input =
{pos_in : unit -> int;
seek_in : int -> unit;
input_char : unit -> char;
in_channel_length : int}

let input_of_string s =
  let pos = ref 0 in 
  {pos_in = (fun () -> !pos);
  seek_in = 
  (fun p ->
    if p < 0 
      then raise (Invalid_argument "seek before beginning")
      else pos := p);
  input_char = 
  (fun () -> 
    if !pos < String.length s
      then (let c = s.[!pos] in pos := !pos +1; c)
      else raise End_of_file);
  in_channel_length = String.length s}

type output =
{output_char : char -> unit;
out_channel_length : unit -> int}

(* 1. Write a function to build an input from an array of characters. *)
(* my version *)
let input_of_array arr =
  let pos = ref 0 in
  {pos_in = (fun () -> !pos);
  seek_in = 
            (fun p -> 
              if p < 0 
                then raise (Invalid_argument "seek before beginning");
              pos := p);
  input_char = (fun () ->
                  if !pos < Array.length arr
                    then (let c = arr.(!pos) in  pos := !pos +1; c)
                  else raise End_of_file);
  in_channel_length = Array.length arr}

(* author's version - basically same *)

(* 2. Write a function input_string of type input → int → string which returns the given number of
characters from the input as a string, or fewer if the input has ended. *)
(* my version *)
let rec input_string_inner i n b =
  match n with
  0 -> Buffer.contents b
  |_ -> try 
        (Buffer.add_char b (i.input_char ()); input_string_inner i (n-1) b)
      with End_of_file -> Buffer.contents b

let input_string i n = input_string_inner i n (Buffer.create n)

(* author's version *)
let input_string i n =
  let b = Buffer.create 100 in
    try
      for x = 0 to n - 1 do
        Buffer.add_char b (i.input_char ())
      done;
      Buffer.contents b
    with
      End_of_file -> Buffer.contents b

(* 3. Extend the input type to include a function input_char_opt which returns a value of type char
option, with None signalling end of file. Extend the functions input_of_channel and input_of_-
string appropriately. *)
(* my version *)
type input =
{pos_in : unit -> int;
seek_in : int -> unit;
input_char_opt : unit -> char option;
in_channel_length : int}

let input_of_channel ch =
  {pos_in = (fun () -> pos_in ch);
  seek_in = 
    (fun p ->
      if p < 0 
        then raise (Invalid_argument "seek before beginning");
      seek_in ch p);
  input_char_opt = 
    (fun () ->
      try 
        Some (input_char ch)
      with End_of_file -> None);
  in_channel_length = in_channel_length ch}

let input_of_string s =
  let pos = ref 0 in 
  {pos_in = (fun () -> !pos);
  seek_in = 
    (fun p ->
      if p < 0
        then raise (Invalid_argument "seek before beginning");
      pos := p);
  input_char_opt = 
    (fun () ->
      if !pos < String.length s
        then (let c = Some s.[!pos] in pos := !pos +1; c)
        else None);
  in_channel_length = String.length s}

(* author's version - basically same - except that it has both input_char and input_char_opt *)

(* 4. Extend the input type with a function input_byte which returns an integer representing the next
byte, or the special value −1 at end of file. Comment on the usefulness of this compared with
input_char_opt and input_char. *)
(* my version *)
type input =
{pos_in : unit -> int;
seek_in : int -> unit;
input_char : unit -> char;
input_byte : unit -> int;
in_channel_length : int}

let input_of_channel ch =
  {pos_in = (fun () -> pos_in ch);
  seek_in = (seek_in ch);
  input_char = (fun () -> input_char ch);
  input_byte = 
    (fun () ->
      try 
        (input_byte ch)
      with End_of_file -> -1);
  in_channel_length = in_channel_length ch}

let input_of_string s =
  let pos = ref 0 in 
  {pos_in = (fun () -> !pos);
  seek_in = 
    (fun p ->
      if p < 0 
        then raise (Invalid_argument "seek before beginning")
        else pos := p);
  input_char = 
    (fun () -> 
      if !pos < String.length s
        then (let c = s.[!pos] in pos := !pos + 1; c)
        else raise End_of_file);
  input_byte = 
    (fun () -> 
      if !pos < String.length s
        then (let n = Char.code (s.[!pos]) in pos := !pos + 1; n)
        else -1);
  in_channel_length = String.length s}

(* author's version *)
type input =
{pos_in : unit -> int;
seek_in : int -> unit;
input_char : unit -> char;
input_byte : unit -> int;
in_channel_length : int}

let no_more = (-1)

let input_of_channel ch =
  {pos_in = (fun () -> pos_in ch);
  seek_in = seek_in ch;
  input_char = (fun () -> input_char ch);
  input_byte =
    (fun () ->
    try int_of_char (input_char ch) with End_of_file -> no_more);
  in_channel_length = in_channel_length ch}
let input_of_string s =
  let pos = ref 0 in
    {pos_in = (fun () -> !pos);
    seek_in =
      (fun p ->
        if p < 0 then raise (Invalid_argument "seek < 0");
        pos := p);
    input_char =
      (fun () ->
        if !pos > String.length s - 1
          then raise End_of_file
          else (let c = s.[!pos] in pos := !pos + 1; c));
    input_byte =
      (fun () ->
        if !pos > String.length s - 1
          then no_more
          else
            (let c = s.[!pos] in pos := !pos + 1; int_of_char c));
    in_channel_length = String.length s}

(* 5. Write an input type which raises End_of_file if it reaches a new line (a '\n' character). Use this to
build a program which reads a line from standard input. *)
(* my version *)
let input_of_string s =
  let pos = ref 0 in
  {pos_in = (fun () -> !pos);
  seek_in = 
    (fun p ->
      if p < 0
        then raise (Invalid_argument "seek before beginning")
        else pos := p);
  input_char = 
    (fun () -> 
      if !pos < String.length s
        then (let c = s.[!pos] in pos := !pos + 1;
          if c = '\n' then raise End_of_file
            else c)
        else raise End_of_file);
  in_channel_length = String.length s}

let input_of_channel ch =
  {pos_in = (fun () -> pos_in ch);
  seek_in = (seek_in ch);
  input_char = 
    (fun () ->
      let c = input_char ch in 
      if c = '\n' 
        then raise End_of_file
        else c);
  in_channel_length = in_channel_length ch}

let rec readlineInner i b =
  try
    (let c = i.input_char () in
    Buffer.add_char b c;
    readlineInner i b)
  with End_of_file -> Buffer.contents b

let readline i = readlineInner i (Buffer.create 80)

(* author's version - unable to run - Exception: Sys_error "Illegal seek" is raised - see problemWithAuthorsCode.ml *)

(* 6. Write a function to build an output from a Buffer.t. Show how this can be used to retrieve a final
string after output is finished. *)
(* my version *)
let output_of_buffer b =
  {output_char = (fun c -> Buffer.add_char b c);
  out_channel_length = (fun () -> Buffer.length b)}

let build_buffer () =
  let b = Buffer.create 16 in 
  let o = output_of_buffer b in
  o.output_char 'I';
  o.output_char 'n';
  o.output_char 'd';
  o.output_char 'u';
  o.output_char 'a';
  o.output_char 'd';
  o.output_char 'i';
  o.output_char 't';
  o.output_char 'y';
  o.output_char 'a';
  Buffer.contents b