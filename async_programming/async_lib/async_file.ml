open Core
open Async

(* copy data from reader to writer, using the provided buffer as scratch
   space *)
(* Core_kernel__Bytes.t -> Reader.t -> Writer.t -> unit Deferred.t = <fun> *)
let rec copy_blocks_a buffer r w =
    Reader.read r buffer
    >>= function
    | `Eof -> return ()
    | `Ok bytes_read ->
        buffer |> Bytes.to_string |> Writer.write w ~len:bytes_read;
        Writer.flushed w
        >>= fun () ->
        copy_blocks_a buffer r w

(* `(>>>)` : 'a Deferred.t -> ('a -> unit) -> unit = <fun>
   Core_kernel__Bytes.t -> Reader.t -> Writer.t -> unit = <fun> *)
let rec copy_blocks_b buffer r w =
    Reader.read r buffer
    >>> function
    | `Eof -> ()
    | `Ok bytes_read ->
        buffer |> Bytes.to_string |> Writer.write w ~len:bytes_read;
        Writer.flushed w |> ignore;
        copy_blocks_b buffer r w

let print_file_contents_a filename =
    let buf = Bytes.create 8 in
    let rd = Reader.open_file filename in
    let w = Writer.stdout |> Lazy.force in
    rd
    >>= fun r -> copy_blocks_a buf r w

let print_file_contents_b filename =
    let buf = Bytes.create 8 in
    let rd = Reader.open_file filename in
    let w = Writer.stdout |> Lazy.force in
    rd
    >>> fun r -> copy_blocks_b buf r w
    (* use `(>>|)` if we want to retern deferred unit
       `(>>>)` will return a plain unit *)

let _ =
    print_file_contents_a "/etc/hosts" |> ignore;
    (* if we don't want to use ignore, then we could've done:
            print_file_contents_a "/etc/hosts"
            >>> never_returns (Scheduler.go ())
       this is because `print_file_contents_a` returns a deferred unit *)
    print_file_contents_b "/tmp/test.txt";
    never_returns (Scheduler.go ())
