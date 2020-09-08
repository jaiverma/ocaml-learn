(* Accessing lists *)

let rec sum lst =
    match lst with
    | [] -> 0
    | h::t -> h + sum t

let rec sum xs =
    match xs with
    | [] -> 0
    | x::xs' -> x + sum xs'

let rec length xs =
    match xs with
    | [] -> 0
    | _::xs' -> 1 + length xs'

(* eq. to [1;2] @ [3;4] *)
let rec append lst1 lst2 =
    match lst1 with
    | [] -> lst2
    | h::t -> h::(append t lst2)

let empty lst =
    lst = []

let _ = sum [1; 2; 3] |> print_int
