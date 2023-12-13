module Cards = Map.Make (Char)

exception Found of int

type hand = { cards : string; bid : int }

let card_types =
  [ 'A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2' ]

let card_types_joker = List.filter (fun c -> c <> 'J') card_types @ [ 'J' ]

let hand_of_line line =
  Utils.split " " line |> fun ts ->
  { cards = List.hd ts; bid = int_of_string @@ List.nth ts 1 }

let new_map () = List.map (fun c -> (c, 0)) card_types |> Cards.of_list
let add map key = Cards.add key (Cards.find key map + 1) map

let map_of_hand hand =
  let map = new_map () in
  let cards = String.to_seq hand |> List.of_seq in
  List.fold_left add map cards

let counts filter hand =
  let hand = map_of_hand hand in
  Cards.filter filter hand |> Cards.to_list |> List.split |> snd
  |> List.sort Int.compare

let full_power hand =
  let counts = counts (fun _ count -> count <> 0) hand in
  match counts with
  | [ 5 ] -> 7 (* five *)
  | [ 1; 4 ] -> 6 (* four *)
  | [ 2; 3 ] -> 5 (* full_house *)
  | [ 1; 1; 3 ] -> 4 (* three *)
  | [ 1; 2; 2 ] -> 3 (* two_pairs *)
  | [ 1; 1; 1; 2 ] -> 2 (* pair *)
  | [ 1; 1; 1; 1; 1 ] -> 1 (* elder *)
  | _ -> 0

let power hand =
  let counts = counts (fun card count -> card <> 'J' && count <> 0) hand in
  match counts with
  | [] | [ 1 ] | [ 2 ] | [ 3 ] | [ 4 ] -> 7 (* five *)
  | [ 1; 1 ] | [ 1; 2 ] | [ 1; 3 ] -> 6 (* four *)
  | [ 1; 1; 1 ] | [ 1; 1; 2 ] -> 4 (* three *)
  | [ 1; 1; 1; 1 ] -> 2 (* pair *)
  | [ 2; 2 ] -> 5 (* full_house *)
  | _ -> full_power hand (* no jokers *)

let compare_strings f s types =
  let rec aux i =
    let fi = List.find_index (fun t -> t = f.[i]) types in
    let si = List.find_index (fun t -> t = s.[i]) types in
    match compare fi si with 0 -> aux (i + 1) | a -> raise (Found (-a))
  in
  try aux 0 with Found a -> a

let compare power_func card_type first second =
  match Int.compare (power_func first.cards) (power_func second.cards) with
  | 0 -> compare_strings first.cards second.cards card_type
  | a -> a

let process power types lines =
  lines |> List.map hand_of_line
  |> List.sort (compare power types)
  |> List.mapi (fun i h -> (i + 1) * h.bid)
  |> List.fold_left ( + ) 0

let first = process full_power card_types
let second = process power card_types_joker
