(* tail recursion *)

let rec sum (l : int list) : int =
    match l with
    | [] -> 0
    | x::xs ->  x + (sum xs)


let rec sum_acc (acc : int) (l : int list) : int =
    match l with
    | [] -> acc
    | x::xs -> sum_acc (acc + x) xs

let sum_tr (l : int list) : int =
    sum_acc 0 l

(* another way of writing without using 'match'
   this only works when 'match' is supposed to be in the first line
   and here we also omit the argument name *)
let rec sum = function
    | [] -> 0
    | x::xs -> x + (sum xs)
