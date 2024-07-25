(* 6. Write a function to reverse the elements of an array in place (i.e. do not create a new array). *)

(* my version *)
let reverse arr =
  let l = Array.length arr in 
  let half = l / 2 in 
  for i = 0 to half do 
    let temp = ref arr.(i) in 
    arr.(i) <- arr.((l-i-1));
    arr.((l-i-1)) <- !temp;
  done

(* author's version *)
let array_rev a =
  if Array.length a > 1 then
    for x = 0 to Array.length a / 2 - 1 do
      let t = a.(x) in
        a.(x) <- a.(Array.length a - 1 - x);
        a.(Array.length a - 1 - x) <- t
  done

(* 7. Write a function table which, given an integer, builds the int array array representing the multipli-
cation table up to that number. For example, table 5 should yield: 
    1     2     3     4     5
    2     4     6     8     10
    3     6     9     12    15
    4     8     12    16    20
    5     10    15    20    25
    *)
(* my version - incorrect one *)
let table n =
  let arr = Array.make n (Array.make n 0) in (* this where I made mistake. a/o to doc "initially physically equal to x (in the sense of the == predicate). Consequently, if x is mutable, it is shared among all elements of the array, and modifying x through one of the array entries will modify all other entries at the same time." so I have initialize each of inner array separately *)
  for i = 0 to (n-1) do 
    for j = 0 to (n-1) do
      arr.(i).(j) <- (i+1) * (j+1)
    done
  done;
  arr

(* my version - corrected *)
let table n =
  let arr = Array.make n [||] in
  for i = 0 to (n-1) do 
    arr.(i) <- Array.make n 0
  done;
  for i = 0 to (n-1) do 
    for j = 0 to (n-1) do 
      arr.(i).(j) <- (i+1) * (j+1)
    done
  done;
  arr

(* author's version which looks exactly like mine except for names *)

let table n =
  let a = Array.make n [||] in
    for x = 0 to n - 1 do
      a.(x) <- Array.make n 0
    done;
  for y = 0 to n - 1 do
    for x = 0 to n - 1 do
    a.(x).(y) <- (x + 1) * (y + 1)
    done
  done;
  a

(* 8. The ASCII codes for the lower case letters 'a'. . . 'z' are 97. . . 122, and for the upper case letters
'A'. . . 'Z' they are 65. . . 90. Use the built-in functions int_of_char and char_of_int to write func-
tions to uppercase and lowercase a character. Non-alphabetic characters should remain unaltered. *)

(* my version *)
let upper c =
  let n = int_of_char c in
  if n >= 97 && n <= 122 then
    char_of_int (n-32)
  else
    c
let lower c = 
  let n = int_of_char c in 
  if n >= 65 && n <= 90 then
    char_of_int (n+32)
  else
    c 

(* author's version is exactly same, so omitted *)

