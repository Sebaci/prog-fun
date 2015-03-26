(* Sebastian Cielemêcki *)

type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;
type 'a lBT = LLEmpty | LLCons of 'a * ('a lBT Lazy.t) * ('a lBT Lazy.t);;

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

(* 1 *)
let lrepeat f list =
  let rec lrepeat1 i =
    function
    | LNil -> LNil
    | LCons (head, tail) ->
       let rec aux =
	 function
	 | 0 -> lrepeat1 (i+1) (tail())
	 | k -> LCons (head, function () -> aux (k-1) )
       in aux (f i)
  in lrepeat1 0 list

let incr n = n+1;;
let id n = n;;
       
ltake (50, (lrepeat incr (first 5)) );;
ltake (15, (lrepeat id (lfrom 4)) );;
