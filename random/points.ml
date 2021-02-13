(* Abstract Data Types
OCaml modules can contain type components as well as value components. *)

open Core

module type POINT = sig
    type point
    val make : int -> int -> point
    val getX : point -> int
    val getY : point -> int
    val origin : point
end

module PairPoint : POINT = struct
    (* PairPoint.point type is abstracted and can't be used as a normal pair
    type *)
    type point = int * int
    let make x y = (x,y)
    let getX (x,_) = x
    let getY (_,y) = y
    let origin = (0,0)
end

let test_pair_point () =
    let p = PairPoint.make 1 2 in
    printf "p.x = %d\n" (PairPoint.getX p);
    printf "p.y = %d\n" (PairPoint.getY p);
    printf "origin = (%d,%d)\n" (PairPoint.getX PairPoint.origin)
        (PairPoint.getY PairPoint.origin)

module ListPoint : POINT = struct
    type point = int list
    let make x y = [x; y]
    let getX p = match p with
        | x::_::[] -> x
        | _ -> failwith "..."
    let getY p = match p with
        | _::y::[] -> y
        | _ -> failwith "..."
    let origin = [0; 0]
end

let test_list_point () =
    let p = ListPoint.make 1 2 in
    printf "p.x = %d\n" (ListPoint.getX p);
    printf "p.y = %d\n" (ListPoint.getY p);
    printf "origin = (%d,%d)\n" (ListPoint.getX ListPoint.origin)
        (ListPoint.getY ListPoint.origin)

(* Functors
Abstract over the particular structure that is used to implement a given
signature. For example, we might want to implement some point functions (adding
points, subtracting points, etc.) in terms of the POINT signature.

Because these operations can be written in terms of the abstract point
operations, we want to be able to specify them in a way that is independent of
the concrete representation of any particular implementation of the POINT
signature *)

module PointOps =
    functor (P: POINT) -> struct
        let neg p = P.make (-(P.getX p)) (-(P.getY p))
        let add p1 p2 = P.make ((P.getX p1) + (P.getX p2))
            ((P.getY p1) + (P.getY p2))
        let sub p1 p2 = add p1 (neg p2)
end

(* PointOps is a functor that takes as its single argument any structure P
satisfying the POINT signature. As its result, it returns a structure that
declares four point functions.*)

module PairPointOps = PointOps(PairPoint)

let test_functor () =
    let p1 = PairPoint.make 1 2 in
    let p2 = PairPoint.make 3 4 in
    let sum = PairPointOps.add p1 p2 in
    let diff = PairPointOps.sub p1 p2 in
    printf "sum  : (%d,%d)\n" (PairPoint.getX sum) (PairPoint.getY sum);
    printf "diff : (%d,%d)\n" (PairPoint.getX diff) (PairPoint.getY diff)

let () =
    test_pair_point ();
    test_list_point ();
    test_functor ()
