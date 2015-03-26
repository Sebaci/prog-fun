(* Sebastian Cielemęcki *)

(* 1 (dom) *)
#load "str.cma"

let zgadnij() = 
	Random.self_init();

	let target_number = string_of_int ( Random.int 101 )
		and guesses = ref []
		and guess = ref "100"
		and try_no = ref 1
		and guessed = ref false
		
	in
	let num_lt str1 str2 =
		let len1 = String.length str1 and len2 = String.length str2 in
		if len1 = len2 then str1 < str2
		else len1 < len2
	in
	print_string "Zgadnij liczbę (0-100): ";
	
	while not !guessed do
		if !guesses = []
		then
			let guess_string = read_line()
			in
			guesses := ( Str.split (Str.regexp "[ \t]+") guess_string );
		else
		begin
			guess := (List.hd !guesses);
			
			print_string "Próba ";
			print_int !try_no;
			try_no := !try_no + 1;
			print_string (": " ^ (!guess) ^ " -- ");
			
			if !guess = target_number then guessed := true
			else
			begin
				guesses := List.tl !guesses;
				
				if num_lt !guess target_number
				then print_endline "moja jest większa!"
				else print_endline "moja jest mniejsza!"
			end;
		end;
	done;
	print_endline "Zgadłeś, brawo!";;
			
(*
zgadnij();;
*)

(* 2 (dom) *)
let sortuj_plik() = 
	print_string "Podaj nazwe pliku do posortowania: ";
	let file_name = read_line() in
	let file = open_in file_name in
	
	let count = ref ( int_of_string ( input_line file ) )
	and numbers = ref [] in
	let rec qsort list = match list with
		[] -> []
		| [x] -> [x]
		| (x::xs) ->  
			let left = List.filter (fun y -> y <= x) (xs)
			and right = List.filter (fun y -> y > x) xs
			in List.append (qsort left) (x::qsort right)
	in
	while !count > 0 do
		numbers := (float_of_string ( input_line file) )::!numbers;
		count := !count-1;
	done;
	close_in file;
	print_string "Podaj nazwe pliku wyjsciowego: ";
	let output_file = read_line()
	and sorted = ref (qsort !numbers) in

	let file_out = open_out output_file
	in
	while !sorted <> [] do
		Printf.fprintf file_out "%f\n" (List.hd !sorted);
		sorted := List.tl !sorted;
	done;
	close_out file_out;;

(*			
sortuj_plik();;

*)
	
