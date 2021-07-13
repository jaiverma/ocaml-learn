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

let make_node name = Node {
        name;
        value = 0;
        children = [];
        subtree_weight = None;
        root_weight = None
    }

let render ~(g: 'a tree) =
    let oc = open_out "/tmp/g.dot" in
    output_string oc "digraph {\n";

    let rec render_helper g =
        match g with
        | Leaf -> failwith "error!"
        | Node n ->
            List.iter (fun child ->
                match child with
                | Leaf -> failwith "error!"
                | Node c -> (
                    output_string oc
                    @@ Printf.sprintf "\t%d -> %d\n" n.name c.name)) n.children;
            List.iter render_helper n.children
    in
    render_helper g;
    output_string oc "}\n";
    close_out oc

let rec add_node ~(g: 'a tree) ~(node: 'a tree) ~(parent: 'a) =
    (* assuming that tree already has a root *)
    match g with
    | Leaf -> failwith "should never happen"
    | Node n -> if n.name = parent then Node { n with children = node :: n.children }
                else Node { n with children = List.map (fun child ->
                    add_node ~g:child ~node ~parent) n.children }

(* read input from stdin *)
let read_input () =
    let _num_planets = int_of_string @@ input_line stdin in
    let num_wormholes = int_of_string @@ input_line stdin in
    Printf.printf "num: %d\n" num_wormholes;

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
        | 1 -> List.rev @@ (a, b, cost) :: acc
        | n -> read_graph (n - 1) @@ (a, b, cost) :: acc
    in

    read_graph num_wormholes []

let () =
    let nodes = Hashtbl.create 100 in
    let graph = read_input () in
    List.iter (fun (a, b, _c) -> Printf.printf "%d %d\n" a b) graph;

    let g = ref Leaf in
    List.iter (fun (a, b, cost) ->
        let node_a =
            match Hashtbl.find_opt nodes a with
            | Some x -> x
            | None -> make_node a
        in
        let node_b =
            match Hashtbl.find_opt nodes b with
            | Some x -> x
            | None -> make_node b
        in

        (* Assuming A is the parent node *)
        (match !g with
        | Leaf -> g := node_a; g := add_node ~g:(!g) ~node:node_b ~parent:a
        | Node _ -> g := add_node ~g:(!g) ~node:node_b ~parent:a)) graph;

    render ~g:(!g)
