(* Sebastian Cielemêcki *)

type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec lfrom k = LCons (k, function () -> lfrom (k+1));;
let rec ones = LCons (1, function () -> ones);;

let rec ltake = function
    (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x,xf)) -> x::ltake(n-1, xf())
;;

let first k = 
  let rec aux i = 
    if i > k then LNil
    else LCons (i, function () -> aux (i+1))
  in aux 1;;
  
let rec toLazy = function
	| [] -> LNil
	| x::xs -> LCons (x, function () -> toLazy xs);;

(* 2 *)
let indexFilter list llist = 
	let rec aux llist k = function
		| [] -> llist
		| i::xi -> match llist with
			| LNil -> LNil
			| LCons (head, tail) ->
				let ftail = tail() in
				if k = i then aux ftail (k+1) xi
				else LCons (head, function () -> aux ftail (k+1) (i::xi))
	in aux llist 0 (List.sort compare list);;

let l = [1;4;7;2];;
let ll = [0; 2; 44; 6; 8; 10; 12; 14; 16; 18; 20; 22; 24; 26; 28; 30; 32];;
	
ltake (8, (indexFilter l (toLazy ll) ) );;


(* 4 *)
type 'a lBT = LLEmpty | LLCons of 'a * ('a lBT Lazy.t) * ('a lBT Lazy.t);;

let rec toLBST = function
	| [] -> LLEmpty
	| x::xs ->
		let ltx = List.filter (function y -> y <= x) xs
		and gtx = List.filter (function y -> y > x) xs
		in LLCons (x, lazy (toLBST ltx), lazy (toLBST gtx) )
		
let traverse tree =
	let rec aux list = function
		| LLEmpty -> list
		| LLCons (x, lleft, lright) ->
			let list1 = aux list (Lazy.force lleft)
			in aux (x::list1) (Lazy.force lright)
	in List.rev (aux [] tree);;
	
let l = [1;3;6;2;7];;
let ltr = toLBST l;;

traverse ltr;;

let l1 = [5;3;1;2;4;0;9;7;6;8;10];;
(*
        5
   3         9
 1   4     7   10
0 2       6 8
*)
let ltr1 = toLBST l1;;

traverse ltr1;;



