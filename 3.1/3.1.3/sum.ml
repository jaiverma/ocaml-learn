let rec sum = function
    | []    -> 0
    | x::xs -> x + sum xs

let rec sum_fail xs =
    match xs with
    | [] -> 1
    | x::xs -> x + sum xs
