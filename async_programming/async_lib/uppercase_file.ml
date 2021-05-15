open Async

(* Deferred.bind: 'a Deferred.t -> f:('a -> 'b Deferred.t) -> 'b Deferred.t = <fun>
   Takes a deferred type and applies a function to the contents of the deferred
   type. The result of the function should also be a deferred type *)
let uppercase_file_a filename =
    Deferred.bind (Reader.file_contents filename) (fun text ->
        Writer.save filename ~contents:(String.uppercase text))

let uppercase_file_b filename =
    Reader.file_contents filename
    >>= fun text ->
    Writer.save filename ~contents:(String.uppercase text)

(* Deferred.map: 'a Deferred.t -> f:('a -> 'b) -> 'b Deferred.t = <fun>
   Takes a deferred type and applies a function to the contents of the deferred
   type. In this case, the function should return a normal value. Deferred.map
   wraps the normal value in a deferred type by using `return` *)
let count_lines_a filename =
    Reader.file_contents filename
    >>= fun text ->
    String.split_on_char '\n' text |> List.length |> return

let count_lines_b filename =
    Reader.file_contents filename
    >>| fun text ->
    String.split_on_char '\n' text |> List.length

let main () =
    let stdout = Writer.stdout |> Lazy.force in

    count_lines_b "/tmp/test.txt"
    >>| fun num ->
    Printf.sprintf "%d\n" num
    |> Writer.write stdout;
    Writer.flushed stdout

let _ =
    main ();
    Scheduler.go ()
