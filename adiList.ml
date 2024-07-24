let rec drop n l =
  match l with
  h::t ->
    if n=0 then
      l
    else
      drop (n-1) t 
  | _ -> l;;

let rec takeInner a n l =
  match l with
  h::t -> 
    if n=0 then
      a
    else
      takeInner (a@[h]) (n-1) t
  | _ -> l;;

let take n l = takeInner [] n l;;

let rec lenInner a l =
  match l with
  [] -> a
  | h::t -> lenInner (a+1) t;;

let len l = lenInner 0 l;;

let rec reverseInner a l = 
  match l with
  h::t -> reverseInner (h::a) t
  | _ -> a;;

let reverse l = reverseInner [] l;;

let rec drop n l =
  match l with 
    [] -> 
    if n > 0  || n < 0 then 
        raise (Invalid_argument "drop")
    else 
        []
  | h::t ->
    if n = 0 then
      l
    else if n < 0 then 
      raise (Invalid_argument "drop")
    else
      drop (n-1) t;;

let rec take n l = 
  match l with
  [] ->
    if n = 0 then
     []
    else 
     raise (Invalid_argument "take")
  | h::t ->
    if n < 0 then 
     raise (Invalid_argument "take") else
    if n = 0 then 
     [] 
    else 
     h :: take (n - 1) t;;