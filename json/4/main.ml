open Yojson.Safe.Util

type partial = {
    a: string;
    b: int;
} [@@deriving yojson]

let read_json filename =
    filename |> Printf.printf "[+] Reading: %s\n";
    let buf = filename |> Yojson.Safe.from_file in
    let p = buf |> partial_of_yojson in
    match p with
    | Error _ -> failwith "failed to create partial json object"
    | Ok p -> p

let _ =
    let p = read_json "/tmp/partial.json" in
    Printf.printf "a: %s, b: %d\n" p.a p.b
