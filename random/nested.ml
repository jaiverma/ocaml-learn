(* A module has a type which is called its signature. A signature consists of
declarations types between keywords "sig" and "end". *)

(*
module Nested :
    sig
        module Data :
            sig
                val d1 : float list
                val d2 : float list
            end
        module Funs :
            sig
                val f1: float list -> float list
                val f2: float list -> float list
            end
    end
*)

(* use #require "core";; in toplevel *)
open Core

module Nested = struct
    module Data = struct
        let d1 = [1.0; 2.0; 3.0]
        let d2 = [10.0; 20.0; 30.0]
    end
    module Funs = struct
        let f1 l = List.map ~f:(fun x -> x +. 1.0) l
        let f2 l = List.map ~f:(fun x -> x -. 1.0) l
    end
end
