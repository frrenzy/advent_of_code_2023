(* first: change to sliding window for 3 rows only, to eliminate need for full S*P scan *)
class symbol row col value =
  object (self)
    method row = row
    method col = col

    method get_gear_ratio (parts : part list) =
      let filter p = p#is_connected (self :> symbol) in
      let connected_parts = List.filter filter parts in
      match (value, connected_parts) with
      | '*', [ a; b ] -> a#value * b#value
      | _ -> 0

    method print () =
      print_endline @@ "(" ^ string_of_int row ^ "; " ^ string_of_int col ^ ")"
  end

and part row col number =
  object
    val left = match col - String.length number with -1 -> 0 | col' -> col'
    val right = match col with 139 -> 139 | col' -> col' + 1
    val top = match row with 0 -> 0 | row' -> row' - 1
    val bottom = match row with 139 -> 139 | row' -> row' + 1
    method value = int_of_string number

    method is_connected (s : symbol) =
      s#row >= top && s#row <= bottom && s#col <= right && s#col >= left

    method print () =
      print_endline @@ "(" ^ string_of_int top ^ "-" ^ string_of_int bottom
      ^ "; " ^ string_of_int left ^ "-" ^ string_of_int right ^ "): " ^ "'"
      ^ number ^ "'"
  end

let process_row i row =
  let rec aux acc pos symbols parts =
    try
      let char = row.[pos] in
      match (char, acc) with
      | '.', "" -> aux "" (pos + 1) symbols parts
      | '.', _ -> aux "" (pos + 1) symbols (new part i (pos - 1) acc :: parts)
      | digit, acc when digit >= '0' && digit <= '9' ->
          aux (acc ^ Char.escaped digit) (pos + 1) symbols parts
      | _, "" -> aux "" (pos + 1) (new symbol i pos char :: symbols) parts
      | _, _ ->
          aux "" (pos + 1)
            (new symbol i pos char :: symbols)
            (new part i (pos - 1) acc :: parts)
    with Invalid_argument _ -> (
      match acc with
      | "" -> (symbols, parts)
      | _ -> (symbols, new part i (pos - 1) acc :: parts))
  in
  aux "" 0 [] []

let parse lines =
  let symbols_matrix, parts_matrix =
    lines |> List.mapi process_row |> List.split
  in
  let symbols = List.flatten symbols_matrix in
  let parts = List.flatten parts_matrix in
  let filter part = List.exists part#is_connected symbols in
  let connected_parts = List.filter filter parts in
  (symbols, connected_parts)

let first lines =
  let _, parts = parse lines in
  let sum_parts acc part = acc + part#value in
  List.fold_left sum_parts 0 parts

let second lines =
  let symbols, parts = parse lines in
  let sum_gears acc symbol = acc + symbol#get_gear_ratio parts in
  List.fold_left sum_gears 0 symbols
