let parse line =
  line |> Utils.split ":" |> List.tl |> List.hd |> Utils.split " "
  |> List.map String.trim
  |> List.filter (fun t -> String.length t > 0)

let parse2 line = parse line |> String.concat "" |> int_of_string

let interval time distance =
  let d = (time * time) - (4 * distance) in
  let l = int_of_float ((float_of_int time -. sqrt (float_of_int d)) /. 2.) in
  let r = int_of_float ((float_of_int time +. sqrt (float_of_int d)) /. 2.) in
  let r = if (r * r) - (time * r) + distance = 0 then r - 1 else r in
  r - l

let first lines =
  let times = parse @@ List.hd lines |> List.map int_of_string in
  let distances = parse @@ List.hd @@ List.tl lines |> List.map int_of_string in
  List.map2 interval times distances |> List.fold_left ( * ) 1

let second lines =
  let time = parse2 @@ List.hd lines in
  let distance = parse2 @@ List.hd @@ List.tl lines in
  interval time distance
