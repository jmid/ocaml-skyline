(** A divide-and-conquer implementation of the skyline problem in O(n logn) time. *)

(*  split : 'a list -> ('a list * 'a list)  *)
let rec split l = match l with
  | []    -> [],[]
  | [x]   -> [x],[] (* uneven length, put in first *)
  | x::y::rest ->
    let xs,ys = split rest
    in x::xs, y::ys
(* tests:
   split [];;
   split [1];;
   split [1;2];;
   split [1;2;3];;
   split [1;2;3;4];;
*)

(*  overlay : (int * int) list -> (int * int) list -> int -> int -> (int * int * int) list  *)
let rec overlay sk1 sk2 curr_h1 curr_h2 = match sk1,sk2 with
  | [],[]             -> []
  | (x1,h1)::sk1', [] -> (x1,h1,curr_h2)::(overlay sk1' [] h1 curr_h2)
  | [], (x2,h2)::sk2' -> (x2,curr_h1,h2)::(overlay [] sk2' curr_h1 h2)
  | (x1,h1)::sk1', (x2,h2)::sk2' ->
    if x1 < x2
    then (x1,h1,curr_h2)::(overlay sk1' sk2 h1 curr_h2)
    else 
      if x1 > x2
      then (x2,curr_h1,h2)::(overlay sk1 sk2' curr_h1 h2)
      else (* x1 = x2 *)
	(x1,h1,h2)::(overlay sk1' sk2' h1 h2)
(* tests:
  overlay [(2,8);(6,0)] [(1,5);(7,0)] 0 0;;
*)

(*  max_per_point : (int * int * int) list -> (int * int) list  *)
let rec max_per_point = List.map (fun (x,h1,h2) -> (x,max h1 h2))
(* tests:
   max_per_point [(1, 0, 5); (2, 8, 5); (6, 0, 5); (7, 0, 0)];;
*)

(*  clean_up : (int * int) list -> (int * int) list  *)
let rec clean_up l curr_h = match l with
  | [] -> []
  | (x,h)::rest ->
    if h = curr_h (* does point mark a genuine height change? *)
    then clean_up rest curr_h
    else (x,h)::(clean_up rest h)
(* tests:
   clean_up [(1, 5); (2, 8); (6, 5); (7, 0)] 0;;
*)

(*  skyline : (int * int * int) list -> (int * int) list  *)
let rec skyline l = match l with
  | []        -> []
  | [(x,h,l)] -> clean_up [(x,h);(x+l,0)] 0
  | _         -> 
    let first,rest = split l in
    let sk1 = skyline first in
    let sk2 = skyline rest in
    let overlay = overlay sk1 sk2 0 0 in
    let skyline = max_per_point overlay in
    clean_up skyline 0
(* tests:
   skyline [(1,5,6);(2,8,4)];;
   skyline [(1,5,6);(2,3,4)];;
   skyline [(1,5,6);(12,3,4)];;
   skyline [(1,5,6);(2,3,14)];;
*)
