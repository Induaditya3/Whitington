(* 1. Write your own version of the function List.concat. The implementation OCaml provides is not
tail-recursive. Can you write one which is? *)

(* my version *)
let rec concatInner a l =
  match l with
  h::t -> concatInner (a @ h) t
  | [] -> a

let concat l = concatInner [] l

(* author's version *)
let rec concat_tail a l =
  match l with
    [] -> List.rev a
  | h::t -> concat_tail (List.rev h @ a) t

let concat l =
  concat_tail [] l

(* 2. Use List.mem to write a function which returns true only if every list in a bool list list contains
true somewhere in it. *)

(* my version *)
let rec map f l =
  match l with
    [] -> []
  | [x] -> [f x]
  | h::t -> f h :: map f t

let true_mem l = 
  not (List.mem false (map (List.mem true) l))

(* author's version -exactly same except that he used built-in map *)
let all_contain_true l =
  not (List.mem false (List.map (List.mem true) l))

(* 3. Write a function to count the number of exclamation marks in a string, using one or more functions from the String module. *)

(* my version *)
let count_exclamations s =
  let count = ref 0 in
    String.iter (fun x -> if x = '!' then count := !count +1 ) s;
  !count

(* author's version - basically same *)
let count_exclamations s =
  let n = ref 0 in
    String.iter (function '!' -> n := !n + 1 | _ -> ()) s; (* as far as remember this syntax was not introduced *)
  !n

(* 4. Use the String.map function to write a function to return a new copy of a string with all exclamation marks replaced with periods (full stops). *)

(* my version *)
let calm s =
  String.map (function '!' -> '.' | x -> x ) s

(* author's version - almost exactly same - he just used partial application to avoid typing two characters *)

(* 5. Use the String module to write a function which concatenates a list of strings together. *)

let listOfString_concat  =
  String.concat "" 

(* author's version - exactly same *)

(* 6. Do the same with the Buffer module. This will be faster. *)

let listOfString_concat l =
  let b = Buffer.create 16 in
    List.iter (Buffer.add_string b) l;
    Buffer.contents b

(* author's version - basically same - I know where this question comes from - it is from OCaml reference manual itself *)

(* 7. Use the String module to count the number of occurrences of the string "OCaml" within a given string. *)

(* my version *)
let occurrences s str =
  let len = String.length s in
  let c = ref 0 in
  let index = ref 0 in
  try
    while !index < (String.length str- len -1) do
      begin
      index := String.index_from str !index s.[0];
      let substring = String.sub str !index len in
      if String.equal s substring then 
        (c := !c +1 ;
        index := !index + len )
      else 
        index := !index + 1
      end
    done;
    !c
  with Not_found -> !c 

(* author's version *)
let occurrences ss s = 
  if ss = "" then 0 else
    let num = ref 0 in
      let str = ref s in
        while String.length ss <= String.length !str && !str <> "" do
          if String.sub !str 0 (String.length ss) = ss then
          num := !num + 1;
          str := String.sub !str 1 (String.length !str - 1)
        done;
        !num
