module S = Set.Make(Int)

module Arvore = struct
    (* tree representation *)
    type t = Arvore of node

    (* values associated with each tree node *)
    and node = {
        nome: int;
        valor: int;
        mutable subarv_peso: int;
        mutable cam_peso: int
    }

    (* store node name to node *)
    let memory: (int, node) Hashtbl.t = Hashtbl.create 10000

    (* store tree representation (parent, children for each node) *)
    let tree_ds: (int, int list) Hashtbl.t = Hashtbl.create 10000

    (* create node from given `name` and `value` *)
    let fazer_nodo (nome: int) (valor: int) =
        let n = {
            nome;
            valor;
            subarv_peso = 0;
            cam_peso = 0
        } in

        (* store node in hash-table for quick access *)
        Hashtbl.add memory nome n;
        Arvore n

    (* return `node` of `tree` *)
    let get (tree: t) =
        match tree with
        | Arvore n -> n

    (* add new node to tree *)
    let rec add_node (pai: int) (node: t) =
        let n = get node in
        match Hashtbl.find_opt tree_ds pai with
            | None -> Hashtbl.add tree_ds pai [n.nome]
            | Some cs -> Hashtbl.add tree_ds pai @@ n.nome :: cs

    (* the main logic is here, this function goes through each tree node
       and updates the weights *)
    let propagate_weights (arv: int) =
        (* acc holds weights from root to node *)
        let rec pesooo (cur: int) (acc: int) =
            (* get children for `cur` node *)
            let children =
                match Hashtbl.find_opt tree_ds cur with
                | Some x -> x
                | None -> []
            in
            (* get `node` representation for current node *)
            let cur_n = Hashtbl.find memory cur in
            (* for each child, calculate sub-tree root while recursing,
               and update weight from root while returning *)
            match children with
            | [] ->
                cur_n.subarv_peso <- 0;
                cur_n.cam_peso <- acc;
                0
            | cs ->
                (* subtree weight for a `node` is sum of all `node`s present
                   in the subtree *)
                let subarv_peso = List.fold_left (fun i child ->
                    let c_n = Hashtbl.find memory child in
                    c_n.cam_peso <- c_n.valor + acc;
                    let weight = c_n.valor + pesooo child (c_n.valor + acc) in
                    (* we want the subtree path which has maximum weight *)
                    if weight > i then weight else i) 0 cs in
                cur_n.subarv_peso <- subarv_peso;
                subarv_peso
        in
        ignore @@ pesooo arv 0
end


(* read input from stdin *)
let read_input () =
    let num_planetas = int_of_string @@ input_line stdin in
    let num_wormholes = int_of_string @@ input_line stdin in

    (* store planets in a hashtable *)
    let planetas = Hashtbl.create num_planetas in

    let rec read_graph n =
        let data =
            input_line stdin
            |> String.split_on_char ' '
            |> List.map int_of_string
        in
        (match data with
        | a :: b :: cost :: [] ->
            (* storing edges as:
                  (planet A -> [(planet B, cost), (planet C, cost), ...]) *)
            (match Hashtbl.find_opt planetas a with
            | None -> Hashtbl.add planetas a [(b, cost)]
            | Some cs -> Hashtbl.add planetas a @@ (b, cost) :: cs);
            (* storing both edges since this is an undirected graph
               (a -> b) and (b -> a) *)
            (match Hashtbl.find_opt planetas b with
            | None -> Hashtbl.add planetas b [(a, cost)]
            | Some cs -> Hashtbl.add planetas b @@ (a, cost) :: cs)
        | _ -> failwith "data not in expected format");

        match n with
        | 1 -> planetas
        | n -> read_graph (n - 1)
    in

    (num_planetas, read_graph num_wormholes)

let () =
    let num_planetas, gg = read_input () in
    (* create and add root of tree which is node `1` *)
    ignore @@ Arvore.fazer_nodo 1 0;

    let rec build_tree pai table seen =
        (* `Set` to keep track of nodes we have already seen as a key in the
           input hash-table, so that we don't recurse infinitely *)
        let seen = S.add pai seen in
        let children =
            Hashtbl.find table pai
            |> List.filter (fun (n, _) -> not @@ S.mem n seen)
        in
        List.iter
            (fun (node, valor) ->
                Arvore.add_node pai
                @@ Arvore.fazer_nodo node valor;
                build_tree node table seen)
            children
    in

    (* build tree from input data *)
    build_tree 1 gg S.empty;

    (* clear input data since we don't need it anymore *)
    Hashtbl.clear gg;

    (* propagate weights in tree to calculate root weight and subtree weight
       for each node *)
    Arvore.propagate_weights 1;

    (* print solution *)
    let rec print_ans i n =
        if i > n then ()
        else
            (let node = Hashtbl.find Arvore.memory i in
            (* we want to print maximum value b/w subtree weight and
               root weight *)
            Printf.printf "%d\n" @@ max node.subarv_peso node.cam_peso;
            print_ans (i + 1) n)
    in
    print_ans 1 num_planetas;
    flush stdout
