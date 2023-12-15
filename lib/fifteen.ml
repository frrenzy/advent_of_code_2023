module Box = Map.Make (Int)

type op = Remove | Add of int
type cmd = { label : string; op : op }
type lens = { label : string; focal : int }

let parse s =
  match String.contains s '-' with
  | true -> { label = List.hd @@ Utils.split "-" s; op = Remove }
  | false ->
      let tokens = Utils.split "=" s in
      {
        label = List.hd @@ tokens;
        op = Add (int_of_string @@ List.nth tokens 1);
      }

let add l label focal =
  let lens = [ { label; focal } ] in
  match List.find_index (fun { label = label'; _ } -> label = label') l with
  | Some i -> Lists.slice_to l i @ lens @ Lists.slice_from l (i + 1)
  | None -> l @ lens

let remove l label =
  match List.find_index (fun { label = label'; _ } -> label = label') l with
  | Some i -> Lists.slice_to l i @ Lists.slice_from l (i + 1)
  | None -> l

let lens_power box slot focal = (box + 1) * (slot + 1) * focal
let hash = String.fold_left (fun acc c -> (acc + int_of_char c) * 17 mod 256) 0
let first lines = List.hd lines |> Utils.split "," |> List.map hash |> Lists.sum

let second lines =
  let boxes =
    List.hd lines |> Utils.split "," |> List.map parse
    |> List.fold_left
         (fun boxes { label; op } ->
           let key = hash label in
           let box = Box.find key boxes in
           match op with
           | Remove -> Box.add key (remove box label) boxes
           | Add f -> Box.add key (add box label f) boxes)
         (List.init 256 (fun i -> (i, [])) |> Box.of_list)
  in
  Box.fold
    (fun box lenses acc ->
      acc
      + (List.mapi (fun slot { focal; _ } -> lens_power box slot focal) lenses
        |> Lists.sum))
    boxes 0
