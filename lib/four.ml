let points n =
  let rec aux acc n = match n with 0 -> acc | n -> aux (acc * 2) (n - 1) in
  match n with 0 -> 0 | _ -> aux 1 (n - 1)

let trim_game_number line = List.hd @@ List.tl @@ Utils.split ": " line

module Cards = Set.Make (Int)

let get_win_number line =
  let parts = Utils.split " | " line in
  let got_str = List.hd @@ List.tl parts in
  let not_empty s = String.length s > 0 in
  let winning =
    List.hd parts |> Utils.split " " |> List.map String.trim
    |> List.filter not_empty |> List.map int_of_string
  in
  let set = Cards.of_list winning in
  let filter number = Cards.mem (int_of_string number) set in
  Utils.split " " got_str |> List.map String.trim |> List.filter not_empty
  |> List.filter filter |> List.length

let get_line_score line = points @@ get_win_number line

let first lines =
  lines |> List.map trim_game_number |> List.map get_line_score
  |> List.fold_left ( + ) 0

let incr_acc acc pos won =
  let rec aux pos' n =
    match n with
    | 0 -> ()
    | _ ->
        acc.(pos') <- acc.(pos') + acc.(pos);
        aux (pos' + 1) (n - 1)
  in
  aux (pos + 1) won

let second lines =
  let trimmed = List.map trim_game_number lines in
  let win = List.map get_win_number trimmed in
  let card_count = List.length lines in
  let multipliers = Array.make card_count 1 in
  List.iteri (fun i won -> incr_acc multipliers i won) win;
  multipliers |> Array.fold_left ( + ) 0
