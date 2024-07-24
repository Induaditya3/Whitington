(*Q4. Now write the inverse function: given a dictionary, return the pair of two lists – the first containing all the keys, and the second containing all the values *)


let rec mklists l =
  match l with
  [] -> ([], [])
  | (k, v)::more ->
  match mklists more with
  (ks, vs) -> (k :: ks, v :: vs);;

  (*my version*)
let rec reverseInner a l = 
  match l with
  h::t -> reverseInner (h::a) t
  | _ -> a;;

  let reverse l = reverseInner [] l;;  

  let rec undictInner d keys values =
  match d with 
  [] -> reverse keys, reverse values
  |(k,v )::t -> undictInner t (k::keys) (v::values);;

let undict d = undictInner d [] [];;

(*5. Define a function to turn any list of pairs into a dictionary. If duplicate keys are found, the value
associated with the first occurrence of the key should be kept.*)
let rec member e l =
  match l with
  [] -> false
  | h::t -> h = e || member e t;;

let rec dictionary_of_pairs_inner keys_seen l =
  match l with
  [] -> []
  | (k, v)::t ->
  if member k keys_seen
  then dictionary_of_pairs_inner keys_seen t
  else (k, v) :: dictionary_of_pairs_inner (k :: keys_seen) t
  let dictionary_of_pairs l =
  dictionary_of_pairs_inner [] l;;

(*my version*)

let rec checkKeys k d =
  match d with 
  [] -> false
  |(k', v')::t -> if k=k' 
                   then true
                  else checkKeys k t;;


let rec dictor d a =
match d with
[] -> a
|(k,v)::t -> if checkKeys k a
                then dictor t a
                else dictor t (a@ [(k, v)]);;

let dictOfPair d = dictor d [];;