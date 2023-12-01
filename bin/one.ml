let filter_digits s =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  List.filter is_digit @@ List.of_seq @@ String.to_seq s

let get_digits_pair = function
  | [] -> (0, 0)
  | [ x ] -> (Advent.Utils.int x, Advent.Utils.int x)
  | xs ->
      (Advent.Utils.int @@ List.hd xs, Advent.Utils.int @@ Advent.Lists.last xs)

let get_number p =
  let x, y = get_digits_pair p in
  (10 * x) + y

let first lines =
  let sum acc x = acc + x in
  List.fold_left sum 0 @@ List.map get_number @@ List.map filter_digits lines

let nums =
  [
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
    ("zero", 0);
  ]

let nums_regex =
  let all_numbers = String.concat "\\|" @@ List.map fst nums in
  Str.regexp all_numbers

let subst s =
  let num_pair = List.find (fun p -> fst p = Str.matched_string s) nums in
  string_of_int @@ snd num_pair

let replace = Str.global_substitute nums_regex subst

let second lines =
  let sum acc x = acc + x in
  let values =
    List.map get_number @@ List.map filter_digits @@ List.map replace lines
  in
  List.fold_left sum 0 values

let () =
  let lines = Advent.File.read_lines 1 in
  print_endline @@ "first: " ^ string_of_int @@ first lines;
  print_endline @@ "second: " ^ string_of_int @@ second lines
