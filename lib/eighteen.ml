type cmd = { dir : string; len : int }

let dir_of_char = function
  | '0' -> "R"
  | '1' -> "D"
  | '2' -> "L"
  | '3' -> "U"
  | _ -> failwith "bad char"

let parse_one line =
  let tokens = Utils.split " " line in
  { dir = List.hd tokens; len = int_of_string @@ List.nth tokens 1 }

let parse_two line =
  let color = List.nth (Utils.split "#" line) 1 in
  {
    dir = dir_of_char @@ String.get color 5;
    len = Utils.int_of_hex @@ String.sub color 0 5;
  }

let dig (x, y) { dir; len } =
  match dir with
  | "R" -> (x, y + len)
  | "L" -> (x, y - len)
  | "U" -> (x - len, y)
  | "D" -> (x + len, y)
  | _ -> failwith "bad dir"

let process parse lines =
  let cmds = lines |> List.map parse in
  let points =
    List.fold_left (fun acc cmd -> dig (List.hd acc) cmd :: acc) [ (0, 0) ] cmds
  in
  let perimeter = List.fold_left (fun acc { len; _ } -> acc + len) 0 cmds in
  Utils.shoelace points + (perimeter / 2) + 1

let first = process parse_one
let second = process parse_two
