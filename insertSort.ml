let rec insert x l =
  match l with 
  [] -> [x]
 |h::t ->
   if x<=h then
     x::h::t
   else
     h :: insert x t;;

let rec sort l =
  match l with 
  h::t -> insert h (sort t)
  | _ -> l;;

(* above two fuctions could be combined into single one with little loss of readability *)

let rec sort l =
  match l with 
  h::t -> let rec insert i list = 
          match list with
          [] -> [i]
         |f::last -> if i<=f then
                    i::f::last
                  else
                    f::(insert i last) in 
                      insert h (sort t)
 |_ -> l;;