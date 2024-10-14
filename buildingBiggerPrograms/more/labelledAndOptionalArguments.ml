(* 1. The function ArrayLabels.make is not labelled, having type int → α → α array. When might this
cause confusion? Write a labelled version to correct this problem. *)
(* my version - answer to first part - if we create int array we might accidently swap size and value and compiler won't complain 
  - this will create bug where neither length of array nor value it contains is what we wanted *)
let make ~len:l ~value:v =
  Array.make l v

(* author's version - basically same *)

(* 2. When we wrote our fill function with labelled arguments, we wanted to prevent someone
mistakenly swapping the start and length values. Can you find a way to do this without labelled or
optional arguments? *)
(* my version *)
type filler =
{start : int;
length : int}

let fill arr filler v =
  for x = filler.start to filler.start + filler.length - 1 do arr.(x) <- v done

let filled () =
  let a = Array.make 100 "x" in 
  fill a {start = 20; length = 40} "y";
  a

(* author's version *)
type start = Start of int
type length = Length of int

let fill a (Start s) (Length l) v =
  for x = s to s + l - 1 do a.(x) <- v done

let filled () =
  let a = Array.make 100 "x" in 
  fill a (Start 20) (Length 40) "y";
  a

(* 3. Build labelled versions of functions from the Buffer module, choosing which functions and arguments
 to label as appropriate. *)
(* my version *)
let sub b ~off:o ~len:l =
  Buffer.sub b o l

let blit b ~srcoff byte ~dstoff ~len =
  Buffer.blit b srcoff byte dstoff len 

let add_substring b s ~ofs ~len =
  Buffer.add_substring b s ofs len

let add_subbytes  b byte ~ofs ~len =
  Buffer.add_subbytes b byte ofs len 

(* author's version - basically same *)

(* 4. Frequently we use an accumulator to make a function tail-recursive, wrapping it up in another
function to give the initial value of the accumulator. For example, we might write: *)
(* let rec map_inner a f l =
  match l with 
  [] -> List.rev a
  |h::t -> map_inner (f h :: a) f t

let map f l = map_inner [] f l *)
(* Use an optional argument to express this as a single function. *)
(* my version *)
let rec map ?(accumulator = []) f l =
  match l with
  [] -> List.rev accumulator
  |h::t -> map ~accumulator:(f h :: accumulator) f t

(* author's version - exactly same except for name of label *)
