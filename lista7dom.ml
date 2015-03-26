(* Sebastian Cielemęcki *)

(* 1 *)

module type QUEUE_FUN =
sig
  (* Module [QueueFun]: first-in first-out queues *)

  (* This module implements queues (FIFOs)in a functional way. *)

  type 'a t
        (* The type of queues containing elements of type ['a]. *)
  exception Empty of string
        (* Raised when [first] is applied to an empty queue. *)
  val create: unit -> 'a t
        (* Return a new queue, initially empty. *)
  val enqueue: 'a * 'a t -> 'a t
        (* [enqueue x q] adds the element [x] at the end of queue [q]. *)
  val dequeue: 'a t -> 'a t
        (* [dequeue q] removes the first element in queue [q] *)        
  val first: 'a t -> 'a
        (* [first q] returns the first element in queue [q] without removing  
           it from the queue, or raises [Empty] if the queue is empty.*) 
  val isEmpty: 'a t -> bool
        (* [isEmpty q] returns [true] if queue [q] is empty, 
           otherwise returns [false]. *)
end;;


module TQueue : QUEUE_FUN = 
struct
	type 'a t = EmptyQueue | Enqueue of 'a * 'a t
	exception Empty of string
	
	let create() = EmptyQueue
	let enqueue (e, q) = Enqueue (e, q)
	let dequeue = function
		  Enqueue (e, q) -> q
		| EmptyQueue -> EmptyQueue
		
	let first = function
		  Enqueue (e, q) -> e
		| EmptyQueue -> raise (Empty "module TQueue: first")
		
	let isEmpty = function
		  EmptyQueue -> true
		| Enqueue _ -> false
		
end;;

let t1 = TQueue.enqueue (1, TQueue.create()) ;;
let t2 = TQueue.enqueue (2, t1);;
TQueue.isEmpty t2;;
TQueue.first t2;;
TQueue.isEmpty (TQueue.dequeue (TQueue.dequeue t2) );;


(* list representation *)
module LQueue : QUEUE_FUN = 
struct
	type 'a t = 'a list
	exception Empty of string
	
	let create() = []
	let enqueue (x, xs) = x::xs
	let dequeue = function
		  x::xs -> xs
		| [] -> []
	
	let first = function
		  x::xs -> x
		| [] -> raise (Empty "module LQueue: first")
	
	let isEmpty = function
		  [] -> true
		| _::_ -> false

end;;

let l1 = LQueue.enqueue (1, LQueue.create() );;
LQueue.first l1;;
let l2 = LQueue.enqueue (2, l1);;
LQueue.first l2;;
LQueue.isEmpty (LQueue.dequeue (LQueue.dequeue l2));;

(* more efficient queue representation *)
module FQueue : QUEUE_FUN =
struct
	type 'a t = 'a list * 'a list
	exception Empty of string
	
	let create() = ([], [])
	let enqueue = function
		  (e, ([], _)) -> ([e], [])
		| (e, (f, r)) -> (f, e::r)
	let dequeue = function
		  ([x], r) -> (List.rev r, [])
		| (_::f, r) -> (f, r)
		| ([], _) -> ([], [])
		
	let first = function
		  (x::_, _) -> x
		| ([], _) -> raise (Empty "module LQueue: first")
		
	let isEmpty q =
		q = ([], [])
	
end;; 

let f1 = FQueue.enqueue (1, FQueue.create());;
let f2 = FQueue.enqueue (2, f1);;
FQueue.first f2;;
let f3 = FQueue.dequeue f2;;
FQueue.first f3;;
let f4 = FQueue.enqueue (3, f3);;
let f5 = FQueue.dequeue f4;;
FQueue.first f5;;
