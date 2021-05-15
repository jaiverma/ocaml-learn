let rec concat = function
    | [] -> ""
    | x::xs -> x ^ concat xs

let () =
    ["hello"; " "; "world"] |> concat |> Printf.printf "%s\n";
    [] |> concat |> Printf.printf "%s\n";
    ["abc"] |> concat |> Printf.printf "%s\n"
