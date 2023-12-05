type interval = { dest : int; src : int; width : int }
type range = { left : int; right : int }

let range_of_ints l r = { left = l; right = l + r - 1 }
let range_of_pairs (l, r) = range_of_ints (int_of_string l) (int_of_string r)

class map lines =
  object (self)
    val intervals =
      lines
      |> List.map (fun line ->
             line |> Utils.split " " |> List.map int_of_string |> Array.of_list
             |> fun t -> { dest = t.(0); src = t.(1); width = t.(2) })

    val mutable done_ranges : range list = []
    method in_interval key { src; width; _ } = key >= src && key < src + width
    method get_from_interval key { dest; src; _ } = dest - src + key

    method get key =
      try
        List.find (self#in_interval key) intervals |> self#get_from_interval key
      with Not_found -> key

    method split { left; right } { dest; src; width } =
      if src > right || src + width - 1 < left then [ { left; right } ]
      else
        let l = max left src in
        let r = min right (src + width - 1) in
        done_ranges <-
          { left = l - src + dest; right = r - src + dest } :: done_ranges;
        let rems = ref [] in
        if l > left then rems := { left; right = l - 1 } :: !rems;
        if r < right then rems := { left = r + 1; right } :: !rems;
        !rems

    method map_one r =
      List.fold_left
        (fun acc_ranges interval ->
          List.flatten @@ List.map (fun r -> self#split r interval) acc_ranges)
        [ r ] intervals

    method map rs =
      done_ranges <- [];
      let unchanged = List.flatten @@ List.map self#map_one rs in
      done_ranges @ unchanged
  end

let split_to_sections lines =
  let fold acc = function
    | "" -> acc |> Array.to_list |> List.cons [] |> Array.of_list
    | line when String.contains line 'm' -> acc
    | line ->
        acc.(0) <- line :: acc.(0);
        acc
  in
  lines |> List.fold_left fold [||] |> Array.to_list |> List.rev

let first_seeds line = Utils.split " " line |> List.tl |> List.map int_of_string

let second_seeds line =
  Utils.split " " line |> List.tl |> Lists.to_pairs |> List.map range_of_pairs

let first lines =
  let seeds = first_seeds @@ List.hd lines in
  let maps = List.tl lines |> split_to_sections |> List.map (new map) in
  let get_location seed =
    List.fold_left (fun key map -> map#get key) seed maps
  in
  let locations = List.map get_location seeds in
  List.fold_left min Int.max_int locations

let second lines =
  let seeds = second_seeds @@ List.hd lines in
  let maps = List.tl lines |> split_to_sections |> List.map (new map) in
  let final_ranges = List.fold_left (fun acc map -> map#map acc) seeds maps in
  final_ranges |> List.map (fun r -> r.left) |> List.fold_left min Int.max_int
