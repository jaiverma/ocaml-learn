module S = Set.Make(Int)

module Tree = struct
    type 'a t = Tree of 'a * 'a t list

    (* values associated with each node *)
    and 'a node = {
        name: 'a;
        value: int;
        mutable subtree_weight: int;
        mutable root_weight: int
    }

    (* store node name to node *)
    let memory: (int, int node) Hashtbl.t = Hashtbl.create 100

    let get tree =
        match tree with
        | Tree (t, c) -> (t, c)

    let make_node name value =
        let n = {
            name;
            value;
            subtree_weight = 0;
            root_weight = 0
        } in

        Hashtbl.add memory name n;
        Tree (name, [])

    (* this function will silently fail if the parent does not already exist in
       the tree *)
    let rec add_node tree parent node =
        let t, children = get tree in
        if t = parent then Tree (t, node :: children)
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
                let n = Hashtbl.find memory node in
                output_string oc
                @@ Printf.sprintf "\t%d [label=\"%d (%d) (%d)\"]\n"
                    node
                    node
                    n.root_weight
                    n.subtree_weight)
            nodes;

        (* function to render edges recursively *)
        let rec render_node t =
            let parent, children = get t in
            List.iter
                (fun child ->
                    let child, _ = get child in
                    let child_n = Hashtbl.find memory child in
                    output_string oc
                    @@ Printf.sprintf "\t%d -> %d [label=\"%d\"]\n"
                        parent child child_n.value)
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
            let cur_n = Hashtbl.find memory cur in
            match children with
            | [] ->
                cur_n.subtree_weight <- 0;
                cur_n.root_weight <- acc;
                0
            | cs ->
                let subtree_weight = List.fold_left (fun i child ->
                    let c, _ = get child in
                    let c_n = Hashtbl.find memory c in
                    c_n.root_weight <- c_n.value + acc;
                    let weight = c_n.value + propagate child (c_n.value + acc) in
                    if weight > i then weight else i) 0 cs in
                cur_n.subtree_weight <- subtree_weight;
                subtree_weight
        in
        ignore @@ propagate tree 0
end


(* read input from stdin *)
let read_input () =
    let _num_planets = int_of_string @@ input_line stdin in
    let num_wormholes = int_of_string @@ input_line stdin in
    let planets = Hashtbl.create 100 in

    let rec read_graph n =
        let data =
            input_line stdin
            |> String.split_on_char ' '
            |> List.map int_of_string
        in
        (match data with
        | a :: b :: cost :: [] ->
            (match Hashtbl.find_opt planets a with
            | None -> Hashtbl.add planets a [(b, cost)]
            | Some cs -> Hashtbl.add planets a @@ (b, cost) :: cs);
            (match Hashtbl.find_opt planets b with
            | None -> Hashtbl.add planets b [(a, cost)]
            | Some cs -> Hashtbl.add planets b @@ (a, cost) :: cs)
        | _ -> failwith "data not in expected format");

        match n with
        | 1 -> planets
        | n -> read_graph (n - 1)
    in

    read_graph num_wormholes

let () =
    let graph = read_input () in
    let tree = ref @@ Tree.make_node 1 0 in

    let rec build_tree parent table seen =
        match Hashtbl.length table with
        | 0 -> ()
        | _ ->
            let seen = S.add parent seen in
            let children =
                Hashtbl.find table parent
                |> List.filter (fun (n, _) -> not @@ S.mem n seen)
            in
            List.iter
                (fun (node, value) ->
                    tree := Tree.add_node !tree parent
                        @@ Tree.make_node node value;
                    build_tree node table seen)
                children
    in

    build_tree 1 graph S.empty;

    Tree.propagate_weights !tree;
    Tree.render !tree "/tmp/g.dot";

    Tree.preorder !tree
    |> List.sort compare
    |> List.iter (fun node ->
        let n = Hashtbl.find Tree.memory node in
        Printf.printf "%d\n"
        @@ max n.subtree_weight n.root_weight)
