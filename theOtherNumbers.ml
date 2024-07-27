(* 1. Give a function which rounds a positive floating-point number to the nearest whole number,
returning another floating-point number. *)

(* my version *)
let round x =
  if (x -. floor x) >= 0.5 then
    floor x +. 1.
  else floor x

(* author's version *)
let round x =
  let c = ceil x in
    let f = floor x in
      if c -. x <= x -. f then c else f

(* 2. Write a function to find the point equidistant from two given points in two dimensions. *)

let equidistant (x0, y0) (x1, y1) =
  let abcissa = (x0 +. x1) /. 2. in
    let ordinate = (y0 +. y1) /. 2. in 
      (abcissa, ordinate)

(* author's solution is basically same *)

(* 3. Write a function to separate a floating-point number into its whole and fractional parts. Return
them as a tuple of type float × float *)

(* my version - only correct for positive floats *)
let parts x = 
  (floor x, x -. floor x)

(* author's version *)
let rec parts x =
  if x < 0. then
    let a, b = parts (-. x) in (* it is like tuple unpacking *)
      (-. a, b)
  else 
    (floor x, x -. floor x)

(* 4. Write a function star of type float → unit which, given a floating-point number between zero and
one, draws an asterisk to indicate the position. An argument of zero will result in an asterisk in
column one, and an argument of one an asterisk in column fifty. *)

let star x =
  if 0. <= x && x <= 1. then 
    let p = int_of_float (round (x *. 50.)) in 
  for i = 0 to p do
    if i = p then
      print_char '*'
    else
      print_char ' '
    done;
    print_newline ()

(* author's version *)
let star x =
  let i = int_of_float (floor (x *. 50.)) in
    let i' = if i = 50 then 49 else i in
      for x = 1 to i' - 1 do print_char ' ' done;
      print_char '*';
      print_newline ()

(* 5. Now write a function plot which, given a function of type float → float, a range, and a step size,
uses star to draw a graph. For example, assuming the existence of the name pi for π, we might see:
# plot sin 0. pi (pi /. 20.);;

*
     *
          *
              *
                  * 
                     *
                       *
                        *
                        *
                      *  
                    *
                  *
              *
           *
      *
 *
 
 Here, we have plotted the sine function on the range 0 . . . π in steps of size π/20. You can define pi
by calculating 4.0 *. atan 1.0. *)

let pi = 4. *. atan 1.
(* my version *)
let rec plot f lower upper step = 
  let check = upper -. lower in 
    if check > 0. then
      begin
      star (f lower);  plot f (lower +. step) upper step
      end
    else ()

(* author's version *)
let plot f a b dy =
  let pos = ref a in
    while !pos <= b do
      star (f !pos);
      pos := !pos +. dy
    done