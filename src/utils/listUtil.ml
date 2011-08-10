

let random_split (l, len) = 
  let final_xlen = len / 2 in 
  let rec loop source slen x xlen y ylen = match source with 
    | [] -> ((x, xlen), (y, ylen)) 
    | (s::ss) -> 
        if Random.int slen < final_xlen - xlen 
        then loop ss (slen-1) (s::x) (xlen+1) y ylen 
        else loop ss (slen-1) x xlen (s::y) (ylen+1) 
  in loop l len [] 0 [] 0;; 

let shuffle l = 
  let rec shuffle_list' (l, len) = match l with 
    | [] -> [] 
    | [x] -> [x] 
    | xs -> 
        let (a, b) = random_split (l, len) 
        in List.rev_append (shuffle_list' a) (shuffle_list' b) 
  in shuffle_list' (l, List.length l);; 

let rec at l n =
  match l with
    | []    -> invalid_arg "index out of bounds"
    | x::xs -> if n = 0 then x else at xs (n-1)

let rec change_to l n value =
  match l with
    | []    -> invalid_arg "index out of bounds"
    | x::xs -> if n = 0 then value::xs else x::(change_to xs (n-1) value)

let rec remove l value =
  match l with
    | [] -> invalid_arg "not found"
    | x::xs -> if x = value then xs else x::(remove xs value)

let rec nth_map f l n =
  match l with
      [] -> invalid_arg "index out of bounds"
    | x::xs -> if n = 0 then (f x)::xs else x::(nth_map f xs (n-1))
