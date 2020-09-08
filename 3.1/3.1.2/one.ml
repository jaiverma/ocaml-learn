(* Variants - OCaml's version of C enums *)

type day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

let int_of_day (d : day) : int =
    match d with
    | Mon -> 1
    | Tue -> 2
    | Wed -> 3
    | Thu -> 4
    | Fri -> 5
    | Sat -> 6
    | Sun -> 7

let _ = int_of_day Fri |> print_int
