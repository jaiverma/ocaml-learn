open Async

let rec copy_blocks buffer r w =
    Reader.read r buffer
    >>= fun ret ->
    match ret with
    | `Eof -> return ()
    | `Ok bytes_read ->
        buffer |> Bytes.to_string |> Writer.write w ~len:bytes_read;
        Writer.flushed w
        >>= fun () ->
        copy_blocks buffer r w

let run () =
    let host_and_port =
        Tcp.Server.create
            ~on_handler_error:`Raise
            (Tcp.Where_to_listen.of_port 9999)
            (fun _addr r w ->
                let buffer = Bytes.create 1024 in
                copy_blocks buffer r w)
    in
    host_and_port |> ignore

let _ =
    run ();
    Scheduler.go ()
