let div x y = x /. y;;

let rec pow x p =
  if p mod 2 == 0 then
    if p == 0 then 1.0
    else 
      let result = pow x (p / 2)
      in result *. result
  else
    x *. (pow x (p - 1));;
    
    
    
let rec pow1 x p = match p with
  |0 -> 1.0
  |n -> if p mod 2 == 0
    then let result = pow x (p / 2)
    in result *. result
    else x *. (pow x (p - 1));;
    
let rec pow2 x p = match p with
  |0 -> 1.0
  |n when p mod 2 == 0 -> 
    let result = pow x (n / 2) in result *. result
  |n -> x *. (pow x (n - 1));;
  
  
let count str chr = 
  let rec strIter str ct chr i = match i with
    |k when String.length str = k -> ct
    |k when str.[k] = chr -> strIter str (ct+1) chr (k+1)
    |k -> strIter str ct chr (k+1)
  in strIter str 0 chr 0
  
  
  
  
