type point = { x : int; y : int }

let height l h =
  List.map (fun line -> if List.for_all (( = ) '.') line then h else 1) l

let width l w =
  let len = List.length @@ List.hd l in
  let indices = List.init len (fun x -> x) in
  List.map
    (fun i ->
      if List.for_all (fun line -> '.' = List.nth line i) l then w else 1)
    indices

let to_points l =
  List.mapi
    (fun y l ->
      List.mapi
        (fun x char -> match char with '#' -> Some { x; y } | _ -> None)
        l
      |> List.filter Option.is_some |> List.map Option.get)
    l
  |> List.flatten

let count ps heights widths =
  let distance y1 x1 y2 x2 =
    let y = succ @@ min y1 y2 in
    let x = succ @@ min x1 x2 in
    let width' = abs (x2 - x1) in
    let height' = abs (y2 - y1) in
    let dist_w = Lists.sum @@ Lists.slice widths x width' in
    let dist_h = Lists.sum @@ Lists.slice heights y height' in
    dist_h + dist_w
  in
  let count_line { x = x1; y = y1 } =
    List.map (fun { x = x2; y = y2 } -> distance y1 x1 y2 x2) ps
  in
  let double = List.map count_line ps |> List.flatten |> Lists.sum in
  double / 2

let process lines multiplier =
  let chars = lines |> List.map String.to_seq |> List.map List.of_seq in
  let heights = height chars multiplier in
  let widths = width chars multiplier in
  count (to_points chars) heights widths

let first lines = process lines 2
let second lines = process lines 1000000
