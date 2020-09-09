(* Options! *)

let x = Some 42
(* val x : int option = Some 42 *)

let y = None
(* val y : 'a option = None *)

let z : int option = None
(* val z : int option = None *)

let extract (o : int option) : string =
    match o with
    | Some i -> string_of_int i
    | None -> ""

let rec list_max = function
    | [] -> None
    | h::t -> begin
        match list_max t with
        | None -> Some h
        | Some m -> Some (max h m)
    end
