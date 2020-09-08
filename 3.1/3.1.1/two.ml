(* Mutating lists *)

let inc_first xs =
    match xs with
    | [] -> []
    | x::xs' -> (x + 1)::xs'
