module Maps = Map.Make (String)

exception Found of int

let map_of_lines lines =
  let to_pair l = (List.hd l, List.hd @@ List.tl l) in
  let nodes, arms_strings =
    List.map (Utils.split " = (") lines |> List.map to_pair |> List.split
  in
  let arms_of_string s =
    let a, b = Utils.split ", " s |> to_pair in
    (a, String.sub b 0 3)
  in
  arms_strings |> List.map arms_of_string |> List.combine nodes |> Maps.of_list

let traverse_one cmd ~map target start =
  let len = String.length cmd in
  let rec aux i key =
    if String.ends_with key ~suffix:target then raise @@ Found i;
    match cmd.[i mod len] with
    | 'L' -> aux (i + 1) (fst @@ Maps.find key map)
    | 'R' -> aux (i + 1) (snd @@ Maps.find key map)
    | _ -> failwith "Invalid cmd string"
  in
  try aux 0 start with Found pos -> pos

let process traverse lines =
  let cmd = List.hd lines in
  let map = lines |> List.tl |> List.tl |> map_of_lines in
  traverse cmd ~map "ZZZ" "AAA"

let traverse_two cmd ~map _ _ =
  Maps.bindings map |> List.split |> fst
  |> List.filter (String.ends_with ~suffix:"A")
  |> List.map (traverse_one cmd ~map "Z")
  |> List.fold_left Utils.lcm 1

let first = process traverse_one
let second = process traverse_two
