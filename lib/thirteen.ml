type mirror = { v : int; h : int }

exception Not

let to_blocks lines =
  let split acc l =
    match (acc, l) with
    | acc, "" -> [] :: acc
    | acc, l -> ((List.of_seq @@ String.to_seq l) :: List.hd acc) :: List.tl acc
  in
  lines |> List.fold_left split [ [] ] |> List.map List.rev |> List.rev

let is_reflected block pos =
  let len = List.length block in
  let rows_to_match = min pos (len - pos) in
  try
    Lists.range 1 rows_to_match
    |> List.fold_left
         (fun acc i ->
           if not acc then raise Not;
           let f = List.nth block (pos - i) in
           let s = List.nth block (pos + i - 1) in
           acc && f = s)
         true
  with Not -> false

let has_smudge block pos =
  let diff a b =
    List.fold_left2 (fun acc a' b' -> if a' = b' then acc else acc + 1) 0 a b
  in
  let len = List.length block in
  let rows_to_match = min pos (len - pos) in
  try
    Lists.range 1 rows_to_match
    |> List.fold_left
         (fun acc i ->
           if acc > 1 then raise Not;
           let f = List.nth block (pos - i) in
           let s = List.nth block (pos + i - 1) in
           acc + diff f s)
         0
    |> ( = ) 1
  with Not -> false

let to_mirror predicate block =
  let rec aux block' i =
    if i = List.length block' then 0
    else match predicate block' i with true -> i | false -> aux block' (i + 1)
  in
  { h = aux block 1; v = aux (Lists.transpose block) 1 }

let process predicate lines =
  lines |> to_blocks
  |> List.map (to_mirror predicate)
  |> List.fold_left (fun acc { v; h } -> acc + (100 * h) + v) 0

let first = process is_reflected
let second = process has_smudge
