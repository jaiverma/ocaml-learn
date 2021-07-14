module S = Set.Make(Int)

module Arvore = struct
    type t = Arvore of node

    (* values associated with each node *)
    and node = {
        nome: int;
        valor: int;
        mutable subarv_peso: int;
        mutable cam_peso: int
    }

    (* store node nome to node *)
    let memory: (int, node) Hashtbl.t = Hashtbl.create 10000

    let tree_ds: (int, int list) Hashtbl.t = Hashtbl.create 10000

    let fazer_nodo (nome: int) (valor: int) =
        let n = {
            nome;
            valor;
            subarv_peso = 0;
            cam_peso = 0
        } in

        Hashtbl.add memory nome n;
        Arvore n

    let get tree =
        match tree with
        | Arvore n -> n

    (* this function will silently fail if the pai does not already exist in
       the arv *)
    let rec add_node (pai: int) (node: t) =
        let n = get node in
        match Hashtbl.find_opt tree_ds pai with
            | None -> Hashtbl.add tree_ds pai [n.nome]
            | Some cs -> Hashtbl.add tree_ds pai @@ n.nome :: cs

    (* this function is not purely functional since it modifies the gg
       instead of creating a new one *)
    let propagate_weights (arv: int) =
        (* acc holds weights from root to node *)
        let rec pesooo (cur: int) (acc: int) =
            let children =
                match Hashtbl.find_opt tree_ds cur with
                | Some x -> x
                | None -> []
            in
            let cur_n = Hashtbl.find memory cur in
            match children with
            | [] ->
                cur_n.subarv_peso <- 0;
                cur_n.cam_peso <- acc;
                0
            | cs ->
                let subarv_peso = List.fold_left (fun i child ->
                    let c_n = Hashtbl.find memory child in
                    c_n.cam_peso <- c_n.valor + acc;
                    let weight = c_n.valor + pesooo child (c_n.valor + acc) in
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
    let planetas = Hashtbl.create num_planetas in

    let rec read_graph n =
        let data =
            input_line stdin
            |> String.split_on_char ' '
            |> List.map int_of_string
        in
        (match data with
        | a :: b :: cost :: [] ->
            (match Hashtbl.find_opt planetas a with
            | None -> Hashtbl.add planetas a [(b, cost)]
            | Some cs -> Hashtbl.add planetas a @@ (b, cost) :: cs);
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
    ignore @@ Arvore.fazer_nodo 1 0;

    let rec build_tree pai table seen =
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

    build_tree 1 gg S.empty;

    Arvore.propagate_weights 1;

    let rec print_ans i n =
        if i > n then ()
        else
            (let node = Hashtbl.find Arvore.memory i in
            Printf.printf "%d\n" @@ max node.subarv_peso node.cam_peso;
            print_ans (i + 1) n)
    in
    print_ans 1 num_planetas;
    flush stdout
