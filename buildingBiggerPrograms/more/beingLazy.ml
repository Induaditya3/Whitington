(* Some basic definitions *)
type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)
let rec ltake (Cons (h, t)) n =
  match n with
  0 -> []
  |_ -> h:: ltake (t ()) (n-1)
let rec ldrop (Cons (h, t) as l) n =
  match n with
  0 -> l
  |_ -> ldrop (t ()) (n-1)
let rec lmap f (Cons (h, t)) =
  Cons (f h, fun () -> lmap f (t ()))
let rec lseq n =
  Cons (n, fun () -> lseq (n+1))
(* 1. Write the lazy list whose elements are the numbers 1, 2, 4, 8, 16 . . . What is its type? *)
(* my version *)
let rec doubles n =
  Cons (n, fun () -> doubles (n*2))
let doubles_from1 = doubles 1

(* author's version - exactly same - obviously binding names differ *)


(* 2. Write a function to return the nth element of a lazy list where element zero is the head of the list. *)
(* my version *)
let rec nthElement (Cons (h, t)) n =
  match n with
  0 -> h
  |_ -> nthElement (t ()) (n-1)

(* author's version - exactly same *)


(* 3. Write a function which, given a list, returns the lazy list forming a repeated sequence taken
from that list. For example, given the list [1; 2; 3] it should return a lazy list with elements
1, 2, 3, 1, 2, 3, 1, 2 . . . *)
(* my version *)
let rec lrepeating ll =
  let rec lrepeatingInner l =
    match l with
    [] -> lrepeating ll
    |h::t -> Cons (h, fun () -> lrepeatingInner t) in 
    lrepeatingInner ll

(* author's version *)
let rec lrepeating_inner c l =
  match c with
  [] -> raise (Invalid_argument "lrepeating: empty list")
  |[x] -> Cons (x, fun () -> lrepeating_inner l l)
  |h::t -> Cons (h, fun () -> lrepeating_inner t l)
let lrepeating l = lrepeating_inner l l


(* 4. Write a lazy list whose elements are the fibonacci numbers 0, 1, 1, 2, 3, 5, 8 . . . whose first two
elements are zero and one by definition, and each ensuing element is the sum of the previous two. *)
(* my version *)
let rec fibonacciInner a b =
  Cons ((a+b), fun () -> fibonacciInner b (a+b))
let fibonacci = 
  Cons (0, fun () -> Cons (1, fun () -> fibonacciInner 0 1))

(* author's version *)
let rec fibonacci_inner x y =
  Cons (x, fun () -> fibonacci_inner y (x+y))
let fibonacci = fibonacci_inner 0 1


(* 5. Write the function unleave which, given a lazy list, returns two lazy lists, one containing elements
at positions 0, 2, 4, 6 . . . of the original list, and the other containing elements at positions 1, 3, 5, 7 . . . *)
(* my version *)
let rec unleaveInner (Cons (h, t)) =
  match (t ()) with
  Cons (hd, tl) -> Cons (h, fun () -> unleaveInner (tl ()))
let unleave (Cons (h, t) as l) =
  (unleaveInner l), (unleaveInner (t ()))

(* author's version *)
(* Some helper functions *)
let fst (a,_) = a
let snd (_, b) = b
(* Core function *)
let rec unleave (Cons (h, t)) =
  let Cons (h', t') = t () in 
    let tl = t' () in 
      (Cons (h, fun () -> fst (unleave tl)),
       Cons (h', fun () -> snd (unleave tl)));;


(* 6. Alphanumeric labels in documents go A, B, C, . . . , X, Y, Z, AA, AB, . . . , BA, BB, . . . AAA, . . . Write
the lazy list containing strings representing this sequence. You may (mis)use the Standard Library
function Char.escaped to convert a character to a string. *)
(* my version *)
(* importing Str module to use string_before - only valid when running this file from REPL using - #use "beingLazy.ml";; *)
#load "str.cma";;
(* helper function *)
let rec next a =
  let len = String.length a in
  if (String.make len 'Z' = a) then
    String.make (len + 1) 'A'
  else if (a.[len - 1] = 'Z') then
    (next (Str.string_before a (len-1))) ^ "A"
  else
    (Str.string_before a (len-1)) ^ Char.escaped (char_of_int(int_of_char a.[len-1] +1))
(* core function *)
let rec labels s =
  Cons (s, fun () -> labels (next s))
let l_labels = labels "A"

(* author's version *)
let rec letter_string n =
  if n <= 26 then
    Char.escaped (char_of_int (n+64))
  else
    (letter_string ((n-1) / 26)) ^ (letter_string (((n-1) mod 26) + 1))
let alpha =
  lmap letter_string (lseq 1)