(* Cielemêcki Sebastian *)

(* 1 *)
let doublePair (x, y) = (2*x, 2*y);;
doublePair (2, 3);;

let bothArePi (x, y) = x == 3.14 && x == y;;
bothArePi (3.14, 2.1);;
bothArePi (3.14, 3.14);;

let doubleListIfGt0 (list, n) = if n > 0 then list @ list else list;;
doubleListIfGt0 ([1;2;3], 1);;


(* 2 *)

let konce list = match list with
    [] -> (0, 0)
  | [_] -> (0, 0)
  | h::l -> let rec konce1 (first, list) = match list with
		[] -> (first, first)
	      | [last] -> (first, last)
	      | h::l -> konce1 (first, l)
	    in konce1 (h, l);;

konce [1;5;7;5;8];;

(* 3 *)
let rec posortowana list = match list with
    [] -> true
  | [_]-> true
  | x::xs -> let rec posort1 (n, list) = match list with
		 [] -> true
	 | x::xs -> if n > x then false
				    else posort1 (x, xs)
    in posort1 (x, xs);;

posortowana [1;3;5;6;7];;
posortowana [1;1;1];;
posortowana [3;2;1];;
posortowana [1;5;2;4;3;8];;


(* 4 *)
let potegi (n, p) =
  let rec potegi1 (n, x, pow, maxPow) = if pow <= maxPow 
				then x::(potegi1 (n, n*x, pow+1 , maxPow))
				else []
  in potegi1(n, n, 1, p);;

potegi (2, 3);;
potegi (5, 2);;


(* 5 *)
let rec podziel (list, n) = match list with
    [] -> ([], [])
  | x::xs -> let (ys, zs) = podziel (xs, n)
	     in
	     if x <= n then (x::ys, zs) else (ys, x::zs);;

podziel ([1;2;6;4;8], 5);;

(* 7 *)
let zamien (list, n) = 
  let rec podzial (list, n, i) = match list with
    [] -> (list, [])
  | x::xs when n < i -> (list, [])
  | x::xs when n == i -> ([x], xs)
  | x::xs -> let (ys, zs) = podzial (xs, n, i+1)
    in (x::ys, zs)
  in
    let (left, right) = podzial (list, n, 1)
    in right @ left;;
    
zamien ([], 5);;
zamien ([1;2;3], 7);;
zamien ([1;3;5;7;9], 3);;
zamien ([1;3;4], 1);;
