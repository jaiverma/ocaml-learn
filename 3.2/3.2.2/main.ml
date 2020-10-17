(* Association lists
   Similar to Python dictionaries (w/o the performance)
   - Insertion is constant time
   - Search is linear
*)

let d = [ ("rectangle", 4); ("triange", 3); ("dodecagon", 12) ]
(* val d : (string * int) list = ... *)

(* insert a binding from key k to value v in association list d *)
let insert k v d = (k,v)::d
(* val insert : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list = <fun> *)

(* find the value v to which k is bound, if any, in the association list d *)
let rec lookup k = function
    | [] -> None
    | (k',v)::t -> if k=k' then Some v else lookup k t
(* val lookup : 'a -> ('a * 'b) list -> 'b option = <fun> *)

let rec lookup k d =
    match d with
    | [] -> None
    | (k',v)::t -> if k=k' then Some v else lookup k t
(* val lookup : 'a -> ('a * 'b) list -> 'b option = <fun> *)
