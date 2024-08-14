(* 1. Write a function which, given a list of integers representing expenses, removes them from a budget, again represented by an integer. *)
(* my version *)
let remaining_budget expenses_list budget =
  List.fold_left (fun a h -> a - h) budget expenses_list

(* author's version *)
let deduct = List.fold_left (-)


(* 2. Calculate the length of a list using one of the fold_ functions. *)
(* my version *)
let len l = 
  List.fold_left (fun a e -> a + 1) 0 l

(* author's version - basically same *)

(* 3. Use one of the fold_ functions to find the last element of list, if any. Behave sensibly if the list is empty. *)
(* without using fold - it's easy *)
let rec last l = 
  match l with
  [] -> raise Not_found
  |h::t -> if t = [] then h else last t
(* my version - using fold *)
let last l = List.fold_left (fun a e -> Some e ) None l
(* author's version *)
let last l =
  match l with
  [] -> None
  | _ -> Some (List.fold_left (fun _ e -> e) (List.hd l) l)

(* 4. Write a function to reverse a list, using one of the fold_ functions. *)
(* my version *)
let rev l = List.fold_left (fun a e -> e::a ) [] l

(* author's version - exactly same *)

(* 5. Write a version of List.mem using one of the fold_ functions. Now setify can be defined entirely using folds. *)
(* mem function using using fold *)
let rec mem e l =
  match l with
  [] -> false
  | h::t -> h = e || mem e t

(* my version - using fold *)
let mem e l = List.fold_left (fun a h -> e = h || a) false l

(* author's version - exactly same as mine *)

(* 6. Use a fold to write a function which, given a list of non-empty strings representing words, returns a single string where the words are separated by spaces. Comment on its efficiency. *)
(* my version *)
let concat l = 
  List.fold_left (fun a e -> if a = "" then e else a ^ " " ^ e) "" l

(* author's version - exactly same - except for binding names and better style *)
let sentence words =
  List.fold_left
  (fun a e -> if a = "" then e else a ^ " " ^ e)
  ""
  words


(* 7. Use fold_tree to write a function which calculates the maximum depth of a tree. What is its type? *)
(* some helping functionalities *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree
let rec fold_tree f a tr =
  match tr with
  Lf -> a
  | Br(e, l, r) -> f e (fold_tree f a l) (fold_tree f a r)
(* my version *)
let max_depth tr =
  fold_tree
  (fun e l r -> max (1 + l) (1 + r))
  0
  tr

(* author's version *)
let max_depth l =
  fold_tree (fun _ l r -> 1 + max l r) 0 l


(* 8. Compare the time efficiency of one or more of your functions with the system implementation of *)
(* the same function (for example, our fold-based member function vs. List.mem) with regard to both *)
(* computational complexity and actual time taken. *)
(* my version *)
let rec cr n = (* function for creating large list *)
  if n = 0 then [] else n::(cr (n-1))
let large_list = cr 1000
let initial_time = Unix.gettimeofday ();;
List.mem 1 large_list
let final_time = Unix.gettimeofday ();;
print_string "Time taken by List.mem 1 [1000;999;998;...;1] is "; print_float (final_time -. initial_time); print_char '\n';;
let initialT = Unix.gettimeofday ();;
mem 1 large_list
let finalT = Unix.gettimeofday ();;
print_string "Time taken by mem 1 [1000;999;998;...;1] is "; print_float (finalT -. initialT); print_char '\n';;

