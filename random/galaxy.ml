module Tree = struct
    type 'a t = Tree of 'a node * 'a t list

    (* values associated with each node *)
    and 'a node = {
        name: 'a;
        value: int;
        subtree_weight: int option;
        root_weight: int option
    }

    let make_node name =
        Tree ({
            name;
            value = 0;
            subtree_weight = None;
            root_weight = None
        }, [])

    (* this function will silently fail if the parent does not already exist in
       the tree *)
    let rec add_node tree parent node =
        match tree with
        | Tree (t, children) ->
            if t.name = parent then Tree (t, node :: children)
            else Tree (t, List.map (fun child ->
                add_node child parent node) children)

    (* dump graph representation as Dot *)
    let render tree filename =
        let oc = open_out filename in
        output_string oc "digraph {\n";

        let rec render_node t =
            match t with
            | Tree (parent, children) ->
            List.iter
                (fun child ->
                    match child with
                    | Tree (child, _) ->
                    output_string oc
                    @@ Printf.sprintf "\t%d -> %d\n" parent.name child.name)
                children;
            List.iter render_node children
        in

        render_node tree;
        output_string oc "}\n";
        close_out oc
end


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

    let g = ref None in
    List.iter (fun (a, b, cost) ->
        let node_a =
            match Hashtbl.find_opt nodes a with
            | Some x -> x
            | None -> Tree.make_node a
        in
        let node_b =
            match Hashtbl.find_opt nodes b with
            | Some x -> x
            | None -> Tree.make_node b
        in

        (* Assuming A is the parent node *)
        (match !g with
        | None -> g := Some node_a; g := Some (Tree.add_node (Option.get !g) a node_b)
        | Some _ -> g := Some (Tree.add_node (Option.get !g) a node_b ))) graph;

    Tree.render (Option.get !g) "/tmp/g.dot"
