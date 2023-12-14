module Balls = Map.Make (Int)

let positions l =
  let balls = Balls.(empty |> add (-1) 0) in
  let map, _, _ =
    List.fold_left
      (fun (acc, pos, last_border) c ->
        match c with
        | '#' -> (Balls.add pos 0 acc, pos + 1, pos)
        | 'O' ->
            let new_balls =
              Balls.update last_border
                (fun opt ->
                  match opt with Some v' -> Some (v' + 1) | None -> None)
                acc
            in
            (new_balls, pos + 1, last_border)
        | _ -> (acc, pos + 1, last_border))
      (balls, 0, -1) l
  in
  (map, List.length l)

let pressure (m, len) =
  Balls.fold
    (fun border count acc ->
      (((2 * len) - (2 * border) - 1 - count) * count / 2) + acc)
    m 0

let first lines =
  lines |> List.map String.to_seq |> List.map List.of_seq |> Lists.transpose
  |> List.map positions |> List.map pressure |> Lists.sum

let second lines = List.length lines
