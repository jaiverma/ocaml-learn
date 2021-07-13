module Tree = struct
    type 'a t = Tree of 'a node * 'a t list

    (* values associated with each node *)
    and 'a node = {
        name: 'a;
        value: int;
        mutable subtree_weight: int;
        mutable root_weight: int
    }

    let get tree =
        match tree with
        | Tree (t, c) -> (t, c)

    let make_node name value =
        Tree ({
            name;
            value;
            subtree_weight = 0;
            root_weight = 0
        }, [])

    (* this function will silently fail if the parent does not already exist in
       the tree *)
    let rec add_node tree parent node =
        let t, children = get tree in
        if t.name = parent then Tree (t, node :: children)
        else Tree (t, List.map (fun child ->
            add_node child parent node) children)

    let rec preorder tree =
        let node, children = get tree in
        node :: (List.flatten @@ List.map (fun child ->
            preorder child) children)

    (* dump graph representation as Dot *)
    let render tree filename =
        let oc = open_out filename in
        output_string oc "digraph {\n";

        (* render nodes and their weights first *)
        let nodes = preorder tree in
        List.iter
            (fun node ->
                output_string oc
                @@ Printf.sprintf "\t%d [label=\"%d (%d) (%d)\"]\n"
                    node.name
                    node.name
                    node.root_weight
                    node.subtree_weight)
            nodes;

        (* function to render edges recursively *)
        let rec render_node t =
            let parent, children = get t in
            List.iter
                (fun child ->
                    let child, _ = get child in
                    output_string oc
                    @@ Printf.sprintf "\t%d -> %d [label=\"%d\"]\n"
                        parent.name child.name child.value)
                children;
            List.iter render_node children
        in

        render_node tree;
        output_string oc "}\n";
        close_out oc

    (* this function is not purely functional since it modifies the graph
       instead of creating a new one *)
    let propagate_weights tree =
        (* acc holds weights from root to node *)
        let rec propagate current acc =
            let cur, children = get current in
            match children with
            | [] ->
                cur.subtree_weight <- 0;
                cur.root_weight <- acc;
                0
            | cs ->
                let subtree_weight = List.fold_left (fun i child ->
                    let c, _ = get child in
                    c.root_weight <- c.value + acc;
                    let weight = c.value + propagate child (c.value + acc) in
                    if weight > i then weight else i) 0 cs in
                cur.subtree_weight <- subtree_weight;
                subtree_weight
        in
        ignore @@ propagate tree 0
end


(* read input from stdin *)
let read_input () =
    let _num_planets = int_of_string @@ input_line stdin in
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
        | 1 -> List.rev @@ (a, b, cost) :: acc
        | n -> read_graph (n - 1) @@ (a, b, cost) :: acc
    in

    read_graph num_wormholes []

let () =
    let nodes = Hashtbl.create 100 in
    let graph = read_input () in
    let g = ref None in

    List.iter (fun (a, b, cost) ->
        let node_a =
            match Hashtbl.find_opt nodes a with
            | Some x -> x
            | None ->
                let n = Tree.make_node a 0 in
                Hashtbl.add nodes a n;
                n
        in
        let node_b =
            match Hashtbl.find_opt nodes b with
            | Some x -> x
            | None ->
                let n = Tree.make_node b cost in
                Hashtbl.add nodes b n;
                n
        in

        (* Assuming A is always the parent node *)
        (match !g with
        | None ->
            g := Some node_a;
            g := Some (Tree.add_node (Option.get !g) a node_b)
        | Some _ ->
            g := Some (Tree.add_node (Option.get !g) a node_b ))) graph;

    Tree.propagate_weights @@ Option.get !g;
    (* Tree.render (Option.get !g) "/tmp/g.dot"; *)

    Tree.preorder (Option.get !g)
    |> List.sort (fun (a: 'a Tree.node) (b: 'a Tree.node) ->
        compare a.name b.name)
    |> List.iter (fun (node: 'a Tree.node) ->
        Printf.printf "%d\n"
        @@ max node.subtree_weight node.root_weight)
