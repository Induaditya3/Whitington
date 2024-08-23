type input =
{pos_in : unit -> int;
seek_in : int -> unit;
input_char : unit -> char;
in_channel_length : int}
;;
(* type input = {
  pos_in : unit -> int;
  seek_in : int -> unit;
  input_char : unit -> char;
  in_channel_length : int;
} *)
let input_string i n =
  let b = Buffer.create 100 in
    try
      for x = 0 to n - 1 do
        Buffer.add_char b (i.input_char ())
      done;
      Buffer.contents b
    with
      End_of_file -> Buffer.contents b;;
(* val input_string : input -> int -> string = <fun> *)
let single_line_input_of_channel ch =
{pos_in = (fun () -> pos_in ch);
seek_in = seek_in ch;
input_char =
(fun () ->
match input_char ch with '\n' -> raise End_of_file | c -> c);
in_channel_length = in_channel_length ch};;
(* val single_line_input_of_channel : in_channel -> input = <fun> *)
input_string (single_line_input_of_channel stdin) max_int;;
(* Exception: Sys_error "Illegal seek". *)
