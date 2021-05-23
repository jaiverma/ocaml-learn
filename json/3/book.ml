type book = {
    name: string;
    author: string;
    num_pages: int;
    genre: string list;
} [@@deriving yojson]

let write_json_to_file ~(filename: string) (json: Yojson.Safe.t) =
    Printf.printf "[+] Writing to %s\n" filename;
    let oc = filename |> open_out in
    Yojson.Safe.pretty_to_channel oc json;
    close_out oc;
    Printf.printf "[*] Done!\n"

let _ =
    let b1 = {
        name = "Harry Potter and the Goblet of Fire";
        author = "J. K. Rowling";
        num_pages = 521;
        genre = ["fantasy"; "fiction"; "magic"]
    } in

    let b2 = {
        name = "A Thousand Splendid Suns";
        author = "Khaled Hoseini";
        num_pages = 326;
        genre = ["drama"; "fiction"]
    } in

    let b3 = {
        name = "Ghost Wars";
        author = "Steve Coll";
        num_pages = 720;
        genre = ["non-fiction"; "espionage"]
    } in

    b1 |> book_to_yojson |> write_json_to_file ~filename:"./book1.json";
    b2 |> book_to_yojson |> write_json_to_file ~filename:"./book2.json";
    b3 |> book_to_yojson |> write_json_to_file ~filename:"./book3.json";
