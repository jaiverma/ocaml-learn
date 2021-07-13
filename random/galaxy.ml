type 'a tree =
    | Leaf
    | Node of 'a node

and 'a node = {
    name: 'a;
    value: int;
    children:  'a tree list;
    mutable subtree_weight: int option;
    mutable root_weight: int option
}

(* read input from stdin *)
let read_input () =
    let _num_plantes = int_of_string @@ input_line stdin in
    let num_wormholes = int_of_string @@ input_line stdin in

    let rec read_graph n acc =
        let (a, b, cost) =
            let data =
                input_line stdin
                |> String.split_on_char ' '
                |> List.map int_of_string
            in
            match data with
            | a :: b :: cost :: [] -> (a, b, cost)
            | _ -> failwith "data not in expected format"
        in
        match n with
        | 1 -> List.rev acc
        | n -> read_graph (n - 1) @@ (a, b, cost) :: acc
    in

    read_graph num_wormholes []

let () =
    let graph = read_input () in
    List.iter (fun (a, b, cost) ->
        Printf.printf "%d %d %d\n" a b cost)
        graph
