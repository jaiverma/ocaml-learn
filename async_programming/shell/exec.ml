open Core
open Async

(* run binary and return (string, string) result,
   if command succeeds, return (Ok stdout)
   if command fails, return (Err stderr) *)
let run_cmd_and_get_output ~(prog: string) ~(args: string list) =
    Process.create ~prog ~args ()
    >>= fun proc ->
    match proc with
    | Error _ ->
        let msg = prog |> sprintf "failed to run: %s" in
        Error msg |> return
    | Ok p ->
        Process.collect_output_and_wait p
        >>| fun ret ->
        match ret.exit_status with
        | Error _ -> Error ret.stderr
        | Ok () -> Ok ret.stdout

let run_azcopy ~(azcopy: string) ~(cmd: string) ~(args: string list) =
    let new_args = cmd :: args in
    match cmd with
    | "copy" | "remove" -> run_cmd_and_get_output ~prog:azcopy ~args:new_args
    | _ ->
        let msg = cmd |> sprintf "command not supported: %s" in
        Error msg |> return

let build_url ~(prefix: string) ~(path: string) ~(suffix: string) =
    let p = (path |> Filename.concat prefix) ^ suffix in
    Writer.write (Writer.stdout |> Lazy.force) (p ^ "\n") |> ignore;
    Writer.flushed (Writer.stdout |> Lazy.force) |> ignore;
    p

let upload ~(url_builder: (path:string -> string)) ~(src: string) ~(dst: string) =
    let url = url_builder ~path:dst in
    let args = [
        src;
        url;
        "--overwrite=true";
        "--from-to=LocalBlob";
        "--blob-type"; "Detect";
        "--follow-symlinks";
        "--check-length=true";
        "--put-md5";
        "--recursive";
        "--trusted-microsoft-suffixes=;";
    ] in
    run_azcopy ~cmd:"copy" ~args

let download ~(url_builder: (path:string -> string)) ~(src: string) ~(dst: string) =
    let url = url_builder ~path:src in
    let args = [
        url;
        dst;
        "--overwrite=true";
        "--check-md5"; "FailIfDifferent";
        "--from-to=BlobLocal";
        "--recursive";
        "--trusted-microsoft-suffixes=;";
    ] in
    run_azcopy ~cmd:"copy" ~args

let remove ~(url_builder: (path: string -> string)) ~(path: string) =
    let url = url_builder ~path in
    let args = [
        url;
        "--recursive";
        "--trusted-microsoft-suffixes=;";
    ] in
    run_azcopy ~cmd:"remove" ~args

let main () =
    let argv = Sys.get_argv () |> Array.to_list |> List.tl_exn in
    let azcopy = "/opt/azcopy_darwin_amd64_10.10.0/azcopy" in

    let upload_fn = upload ~url_builder ~azcopy in
    let download_fn = download ~url_builder ~azcopy in
    let remove_fn = remove ~url_builder ~azcopy in

    let ret = match argv with
    | command :: args -> (
        match command with
        | "upload" -> (
            match args with
            | src :: dst :: _ -> upload_fn ~src ~dst
            | _ -> failwith "usage: upload <src> <dst>"
        )
        | "download" -> (
            match args with
            | src :: dst :: _ -> download_fn ~src ~dst
            | _ -> failwith "usage: download <src> <dst>"
        )
        | "remove" -> (
            match args with
            | path :: _ -> remove_fn ~path
            | _ -> failwith "usage: remove <path>"
        )
        | cmd -> (
            let msg = cmd |> sprintf "command not implemented: %s" in
            failwith msg
        )
    )
    | _ -> failwith "usage: [upload|download|remove] <args> ..."

    in
    ret
    >>| (function
    | Ok out ->
        let stdout = Writer.stdout |> Lazy.force in
        Writer.write stdout out;
        Writer.flushed stdout
    | Error _ ->
        let stderr = Writer.stderr |> Lazy.force in
        Writer.write stderr "Error!\n";
        Writer.flushed stderr)
    >>> function
    | _ -> shutdown 0

let _ =
    main ();
    never_returns (Scheduler.go ())
