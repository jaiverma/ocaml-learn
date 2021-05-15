let rec product = function
    | [] -> 1
    | x::xs -> x * product xs

let () =
    [1; 2; 3; 4; 5] |> product |> Printf.printf "%d\n";
    [] |> product |> Printf.printf "%d\n";
    [-1; -2; -3; -4; -5] |> product |> Printf.printf "%d\n"
