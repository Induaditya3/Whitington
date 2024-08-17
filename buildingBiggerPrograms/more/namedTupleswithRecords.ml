(* 1. Show how to update a reference without using the := operator. *)
(* my version *)
let x = ref 0;;
x.contents <- 1

(* author's version - exactly same *)


(* 2. Using functions from the “Time Functions” section of the documentation to the Unix module, write
a program which, when run, returns a string containing the time and date, for example "It is
2:45 on Wednesday 8 January 2014". *)
(* my version *)
let clock {Unix.tm_sec; tm_hour = h; tm_min = m; _} =
  (string_of_int h) ^ ":" ^ (string_of_int m)
let week {Unix.tm_sec; tm_wday = w; _} =
  match w with
  0 -> "Sunday"
  |1 -> "Monday"
  |2 -> "Tuesday"
  |3 -> "Wednesday"
  |4 -> "Thursday"
  |5 -> "Friday"
  |_ -> "Saturday"
let day {Unix.tm_sec; tm_mday = m; _} = string_of_int m
let month {Unix.tm_sec; tm_mon = mon; _} =
  match mon with
  0 -> "January"
  |1 -> "February"
  |2 -> "March"
  |3 -> "April"
  |4 -> "May"
  |5 -> "June"
  |6 -> "July"
  |7 -> "August"
  |8 -> "September"
  |9 -> "October"
  |10 -> "November"
  |_ -> "December"
let year {Unix.tm_sec; tm_year = y; _} =
  string_of_int (y + 1900)
let time ({Unix.tm_sec; _} as t) =
  "It is "
  ^ clock t
  ^ " on "
  ^ week t ^ " "
  ^ day t ^ " "
  ^ month t ^ " "
  ^ year t;;
time (Unix.localtime (Unix.gettimeofday ()))

(* author's version *)
let string_of_month m =
  match m with
  0 -> "January"
  | 1 -> "February"
  | 2 -> "March"
  | 3 -> "April"
  | 4 -> "May"
  | 5 -> "June"
  | 6 -> "July"
  | 7 -> "August"
  | 8 -> "September"
  | 9 -> "October"
  | 10 -> "November"
  | 11 -> "December"
  | _ -> raise (Invalid_argument "string_of_month")
let string_of_day d =
  match d with
  0 -> "Sunday"
  | 1 -> "Monday"
  | 2 -> "Tuesday"
  | 3 -> "Wednesday"
  | 4 -> "Thursday"
  | 5 -> "Friday"
  | 6 -> "Saturday"
  | _ -> raise (Invalid_argument "string_of_day")
let string_of_time () =
  let
    {Unix.tm_min;
    Unix.tm_hour;
    Unix.tm_mday;
    Unix.tm_mon;
    Unix.tm_year;
    Unix.tm_wday}
    =
    Unix.localtime (Unix.time ())
    in
      "It is "
      ^ string_of_int tm_hour
      ^ ":"
      ^ string_of_int tm_min
      ^ " on "
      ^ string_of_day tm_wday
      ^ " "
      ^ string_of_int tm_mday
      ^ " "
      ^ string_of_month tm_mon
      ^ " "
      ^ string_of_int (tm_year + 1900)


(* 4. Define a record of six items a...f where a and b have the same type as one another, c and d have the
same type as one another and e and f have the same type as one another. *)
(* author's version - without my version - because I had no idea of this syntax *)
type ('a, 'b, 'c) t = 
  {a : 'a;
  b : 'a;
  c : 'b;
  d : 'b;
  e : 'c;
  f : 'c}

(* 5. Records are used in the module Gc which controls OCaml’s garbage collector (a garbage collector
is a system which automatically reclaims space the program has finished with as the program is
running). Use the data structures and functions in the Gc module to write programs which:
(a) write a summary of the state of the garbage collector to a text file; and
(b) alter the verbosity of the garbage collector as defined in the control record. *)
(* my version - Gc.set part is straight from docs *)
let summarize fname =
  let ch = open_out fname in 
  Gc.print_stat ch;
  close_out ch;;
Gc.set {(Gc.get ()) with Gc.verbose = 0x00d}

(* author's version - didn't fully get the question - so my code and his code does completely different things *)
let write_gc_summary filename =
  let ch = open_out filename in
    let
      {Gc.minor_words;
      Gc.promoted_words;
      Gc.major_words;
      Gc.minor_collections;
      Gc.major_collections}
      =
    Gc.stat ()
    in
    output_string ch "Minor Words: ";
    output_string ch (string_of_float minor_words);
    output_string ch "\nPromoted Words: ";
    output_string ch (string_of_float promoted_words);
    output_string ch "\nMajor Words: ";
    output_string ch (string_of_float major_words);
    output_string ch "\nMinor Collections: ";
    output_string ch (string_of_int minor_collections);
    output_string ch "\nMajor Collections: ";
    output_string ch (string_of_int major_collections);
    close_out ch
let start_of_major = 0x001
let minor_collection = 0x002
let heap_grow_shrink = 0x004
let stack_resizing = 0x008
let heap_compaction = 0x010
let change_parameters = 0x020
let compute_slice_size = 0x040
let call_finalisation = 0x080
let bytecode_exe_search = 0x100
let change_verbosity vs =
  let n = List.fold_left ( + ) 0 vs in
    Gc.set {(Gc.get ()) with Gc.verbose = n}
