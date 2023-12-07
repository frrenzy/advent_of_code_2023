let filter_digits s =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  s |> String.to_seq |> List.of_seq |> List.filter is_digit

let get_digits_pair = function
  | [] -> (0, 0)
  | [ x ] -> (Utils.int x, Utils.int x)
  | xs -> (Utils.int @@ List.hd xs, Utils.int @@ Lists.last xs)

let get_number p =
  let x, y = get_digits_pair p in
  (10 * x) + y

let first lines =
  List.map filter_digits lines |> List.map get_number |> List.fold_left ( + ) 0

let nums =
  [
    ("one", "1e");
    ("two", "2o");
    ("three", "t3e");
    ("four", "f4r");
    ("five", "f5e");
    ("six", "6x");
    ("seven", "s7n");
    ("eight", "e8t");
    ("nine", "n9e");
    ("zero", "z0o");
  ]

let nums_regex = String.concat "\\|" @@ List.map fst nums |> Str.regexp
let subst s = List.find (fun p -> fst p = Str.matched_string s) nums |> snd

let replace s =
  let rec loop str =
    let new_str = Str.substitute_first nums_regex subst str in
    let len = String.length str in
    match String.length new_str with
    | l when l = len -> new_str
    | _ -> loop new_str
  in
  loop s

let second lines = List.map replace lines |> first
