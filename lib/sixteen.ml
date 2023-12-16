type dir = Left | Right | Top | Bottom

let value = function Top -> 1 | Left -> 2 | Right -> 3 | Bottom -> 4
let unvalue = function 1 -> Top | 2 -> Left | 3 -> Right | 4 -> Bottom
let pass d = unvalue (5 - value d)

module Beams = Set.Make (struct
  type t = int * int * dir

  let fst = function a, _, _ -> a
  let snd = function _, b, _ -> b
  let trd = function _, _, c -> c

  let compare a b =
    match compare (fst a) (fst b) with
    | 0 -> (
        match compare (snd a) (snd b) with
        | 0 -> compare (value @@ trd a) (value @@ trd b)
        | v -> v)
    | v -> v
end)

exception Invalid_dir
exception End of Beams.t
exception Cycle

class node symbol =
  object
    method next_dir from =
      match (from, symbol) with
      | _, '.' | (Top | Bottom), '|' | (Left | Right), '-' -> [ pass from ]
      | Top, '-' | Bottom, '-' -> [ Left; Right ]
      | Left, '|' | Right, '|' -> [ Top; Bottom ]
      | Left, '/' | Right, '\\' -> [ Top ]
      | Left, '\\' | Right, '/' -> [ Bottom ]
      | Top, '\\' | Bottom, '/' -> [ Right ]
      | Top, '/' | Bottom, '\\' -> [ Left ]
      | _ -> raise Invalid_dir
  end

let get (a, b) grid = List.nth (List.nth grid a) b
let count b = Beams.map (fun (a, b, _) -> (a, b, Top)) b |> Beams.cardinal

let next a b =
  List.map (fun dir ->
      match dir with
      | Top -> (a - 1, b, Bottom)
      | Bottom -> (a + 1, b, Top)
      | Right -> (a, b + 1, Left)
      | Left -> (a, b - 1, Right))

let cycle a b from (grid : node list list) =
  let rec aux acc beams =
    try
      let a, b, from = try List.hd acc with Failure _ -> raise (End beams) in
      if Beams.mem (a, b, from) beams then raise Cycle;
      let node = get (a, b) grid in
      aux
        (next a b (node#next_dir from) @ List.tl acc)
        (Beams.add (a, b, from) beams)
    with Invalid_argument _ | Failure _ | Cycle -> aux (List.tl acc) beams
  in
  try aux [ (a, b, from) ] Beams.empty with End beams -> beams

let to_grid =
  List.map (fun l -> String.to_seq l |> List.of_seq |> List.map (new node))

let process grid (a, b, from) = cycle a b from grid |> count
let first lines = process (to_grid lines) (0, 0, Left)

let second lines =
  let grid = to_grid lines in
  let len = List.length grid in
  List.init len (fun i ->
      [ (0, i, Top); (len - 1, i, Bottom); (i, 0, Left); (i, len - 1, Right) ])
  |> List.flatten
  |> List.map (process grid)
  |> List.fold_left max 0
