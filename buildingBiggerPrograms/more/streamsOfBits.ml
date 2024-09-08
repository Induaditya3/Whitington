(* some necessary functionalities *)
type input =
{pos_in : unit -> int;
seek_in : int -> unit;
input_char : unit -> char;
in_channel_length : int}

let input_of_channel ch =
{pos_in = (fun () -> pos_in ch);
seek_in = seek_in ch;
input_char = (fun () -> input_char ch);
in_channel_length = in_channel_length ch}

type input_bits =
{input : input;
mutable byte : int;
mutable bit : int}

let input_bits_of_input i =
  {input = i;
  byte = 0;
  bit = 0}

let rec getbit i =
  if i.byte = 0 then
    begin
      i.byte <- int_of_char (i.input.input_char ());
      i.bit <- 128;
      getbit i
    end
  else
    let r = i.byte land i.bit > 0 in
    i.bit <- i.bit / 2;
    if r then 1 else 0

let getval i n =
  if n < 0 || n > 31 then
    raise (Invalid_argument "getval")
  else 
    begin
      let r = ref 0 in
      for x = n-1 downto 0 do
        r := !r lor ((getbit i) lsl x)
      done;
      !r
    end

let align i =
  i.bit <- 0

(* 1. Specialize the function getval so that writing 8 bits at a time when the input is aligned is optimized.
Benchmark this function against the naive one. *)
(* my version - with unnecessary fluff *)
let getval_optimized i n =
  if n = 8 && i.bit = 0 then
    begin
    i.byte <- int_of_char (i.input.input_char ());
    align i;
    i.byte
    end
  else
    getval i n

(* author's version *)
let getval_fast i n =
  if n = 8 && i.bit = 0 then 
    int_of_char (i.input.input_char ())
  else
    getval i n

(* 2. Write the function getval_32 which can get a value of type Int32.t in the same fashion as getval. *)
(* my version *)
let getval_32 i n =
  if n < 0 && n > 32 then
    raise (Invalid_argument "getval_32")
  else
    let r = ref 0l in 
    for x = n-1 downto 0 do 
      let num = Int32.shift_left (Int32.of_int (getbit i)) x in 
      r := Int32.logor !r num
    done;
    !r 

(* author's version *)
let getval_32 b n =
  if n < 0 then raise (Invalid_argument "getval_32") else
    if n = 0 then 0l else
      let r = ref Int32.zero in
        for x = n - 1 downto 0 do
          let num = Int32.of_int (getbit b) in
            r := Int32.logor !r (Int32.shift_left num x)
        done;
        !r

(* Some necessary definitions for output bits *)
type output =
{output_char : char -> unit;
out_channel_length : unit -> int}

type output_bits =
{output : output;
mutable obyte : int;
mutable obit : int}

let output_of_channel ch =
  {output_char = output_char ch;
  out_channel_length = (fun () -> out_channel_length ch)}

let output_bits_of_output o =
  {output = o;
  obyte = 0;
  obit = 7}

let flush o =
  if o.obit < 7 then o.output.output_char (char_of_int o.obyte);
  o.obyte <- 0;
  o.obit <- 7

let rec putbit o b =
  if o.obit = (-1) then
    begin
      flush o;
      putbit o b
    end
  else
    begin
      if b <> 0 then (o.obyte <- o.obyte lor (1 lsl o.obit ));
      o.obit <- o.obit - 1;
    end

let putval o v l =
  for x = l-1 downto 0 do
    putbit o (v land (1 lsl x))
  done

(* 3. Specialize the function putval so that writing 8 bits at a time when the output is aligned is optimized.
Benchmark this function against the naive one. *)
(* my version *)
let putval_optimized o v l =
  if l = 8 && o.obit = 7 then 
    o.output.output_char (char_of_int v)
  else
    putval o v l

(* author's version - exactly same *)

(* 4. Write the function putval_32 which can put a value of type Int32.t in the same fashion as putval. *)
(* my version *)
let putval_32 o v l =
  for x = l-1 downto 0 do
    begin
      let num = Int32.logand v (Int32.shift_left 1l x) in 
      putbit o (Int32.to_int num)
    end
  done

(* author's version - corrected *)
let putval_32 o v l =
  for x = l - 1 downto 0 do
    putbit o
      (Int32.to_int
        (Int32.logand v (Int32.shift_left (Int32.of_int 1) x)))
  done

(* 5. We said that the output_bits type needed a flush operation. In fact, this is not always true – for
outputs built with, for example, output_of_bytes, we could write the current byte every time a
bit is written, seeking back one byte each time, only moving on when the byte is actually finished.
Implement this. *)
(* my version - it has no intermediatary interface between output and output_bits - it operates directly on output *)
let output_of_bytes b =
  let bit = ref 7 in
  let pos = ref 0 in
  {output_char = (fun c ->
    if !bit = (-1) then 
      begin
        pos := !pos +1;
        bit := 7;
        if c <> '\000' then
          Bytes.set b !pos (char_of_int ((int_of_char (Bytes.get b !pos)) lor (1 lsl !bit)));
        bit := !bit -1
      end
    else
      begin
        if c <> '\000' then
          Bytes.set b !pos (char_of_int ((int_of_char (Bytes.get b !pos)) lor (1 lsl !bit)));
        bit := !bit - 1
      end);
  out_channel_length = (fun () -> Bytes.length b)}

let putbit o b =
  if b <> 0 
    then o.output_char (char_of_int 1)
  else o.output_char (char_of_int 0)
  
let putval o v l =
  for x = l-1 downto 0 do
    putbit o (v land (1 lsl x))
  done

(* author's version *)
type output =
{output_char : char -> unit;
rewind : unit -> unit;
out_channel_length : unit -> int }

type output_bits =
{output : output;
mutable obit : int;
mutable obyte : int}

let output_of_bytes b =
  let pos = ref 0 in 
  {output_char = (fun c ->
                    if !pos < Bytes.length b
                      then (Bytes.set b !pos c; pos := !pos + 1)
                    else raise End_of_file );
  rewind = (fun () ->
              if !pos > 0
                then pos := !pos - 1
              else raise (Failure "rewind"));
  out_channel_length = (fun () -> Bytes.length b)}

let rec putbit o b =
  if o.obit = (-1) then 
    begin
      o.obyte <- 0;
      o.obit <- 7;
      putbit o b
    end
  else
    begin
      if b <> 0 then (o.obyte <- o.obyte lor (1 lsl o.obit));
      o.output.output_char (char_of_int o.obyte);
      o.output.rewind ();
      o.obit <- o.obit - 1
    end