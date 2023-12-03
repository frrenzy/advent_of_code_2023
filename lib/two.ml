type round = { green : int; red : int; blue : int; game : int }
type color = Red of int | Green of int | Blue of int

let trim_game_number s =
  let tokens = Utils.split ": " s in
  Lists.get tokens 1

let to_color_list s =
  let tokens = Str.split (Str.regexp_string ", ") s in
  let to_color pick =
    let pick_tokens = Str.split (Str.regexp_string " ") pick in
    match
      (int_of_string @@ List.hd pick_tokens, List.hd @@ List.tl pick_tokens)
    with
    | a, "green" -> Green a
    | a, "red" -> Red a
    | a, "blue" -> Blue a
    | _ -> failwith "invalid color or number"
  in
  List.map to_color tokens

let color_list_to_round l =
  let set_color acc c =
    match (c, acc) with
    | Red a, { red; green; blue; game } -> { red = red + a; green; blue; game }
    | Green a, { red; green; blue; game } ->
        { red; green = green + a; blue; game }
    | Blue a, { red; green; blue; game } ->
        { red; green; blue = blue + a; game }
  in
  List.fold_left set_color { green = 0; red = 0; blue = 0; game = 0 } l

let to_round s = List.map to_color_list s |> List.map color_list_to_round

let to_game i l =
  let set_game acc c =
    match (acc, c) with
    | ( { red = red_acc; blue = blue_acc; green = green_acc; game },
        { red; green; blue; _ } ) ->
        {
          red = max red red_acc;
          green = max green green_acc;
          blue = max blue blue_acc;
          game;
        }
  in
  List.fold_left set_game { green = 0; red = 0; blue = 0; game = i + 1 } l

let is_valid task game =
  match (task, game) with
  | ( { red = red_task; blue = blue_task; green = green_task; _ },
      { red; green; blue; _ } ) ->
      red <= red_task && blue <= blue_task && green <= green_task

let lines_to_games lines =
  lines |> List.map trim_game_number
  |> List.map (Utils.split "; ")
  |> List.map to_round |> List.mapi to_game

let first lines =
  let games = lines_to_games lines in
  let count acc cur_game =
    match is_valid { red = 12; green = 13; blue = 14; game = 0 } cur_game with
    | true -> acc + cur_game.game
    | false -> acc
  in
  List.fold_left count 0 games

let power = function { red; green; blue; _ } -> red * green * blue

let second lines =
  let games = lines_to_games lines in
  let count acc game = acc + power game in
  List.fold_left count 0 games
