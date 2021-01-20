open Core

let () =
    let buf = In_channel.read_all "book.json" in
    let json1 = Yojson.Basic.from_string buf in
    let json2 = Yojson.Basic.from_file "book.json" in
    print_endline (if Yojson.Basic.equal json1 json2 then "OK" else "FAIL")
