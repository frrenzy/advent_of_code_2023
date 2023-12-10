type dir = Top | Bottom | Left | Right | Invalid_dir | Any_dir
type pipe = Pipe of dir * dir | Ground | Start | Any_pipe

exception Invalid_direction

class node symbol =
  object (self)
    method t =
      match symbol with
      | '|' -> Pipe (Top, Bottom)
      | '-' -> Pipe (Left, Right)
      | 'L' -> Pipe (Top, Right)
      | 'J' -> Pipe (Top, Left)
      | '7' -> Pipe (Bottom, Left)
      | 'F' -> Pipe (Bottom, Right)
      | '.' -> Ground
      | 'S' -> Start
      | _ -> Any_pipe

    method symbol =
      match symbol with
      | '|' -> "|"
      | '-' -> "─"
      | 'F' -> "╭"
      | '7' -> "╮"
      | 'J' -> "╯"
      | 'L' -> "╰"
      | _ -> "*"

    method next_dir (from : dir) =
      match (from, self#t) with
      | _, Ground | _, Any_pipe -> Invalid_dir
      | _, Start -> Any_dir
      | from, Pipe (a, b) -> (
          match from with
          | f when f = a -> b
          | f when f = b -> a
          | _ -> Invalid_dir)
  end

let nodes_of_line l = String.to_seq l |> List.of_seq |> List.map (new node)
let get (a, b) grid = List.nth (List.nth grid a) b

let get_cycle (grid : node list list) =
  let rec aux (a, b) from pos acc =
    let node =
      try get (a, b) grid with Failure _ -> raise Invalid_direction
    in
    match node#next_dir from with
    | Top -> aux (a - 1, b) Bottom (pos + 1) ((a, b, node#symbol) :: acc)
    | Bottom -> aux (a + 1, b) Top (pos + 1) ((a, b, node#symbol) :: acc)
    | Right -> aux (a, b + 1) Left (pos + 1) ((a, b, node#symbol) :: acc)
    | Left -> aux (a, b - 1) Right (pos + 1) ((a, b, node#symbol) :: acc)
    | Any_dir -> (a, b, node#symbol) :: acc
    | _ -> raise @@ Invalid_direction
  in
  List.filter_map
    (fun (a, b, from) ->
      try Some (aux (a, b) from 2 []) with Invalid_direction -> None)
    [ (51, 100, Bottom); (53, 100, Top); (52, 99, Right); (52, 101, Left) ]
  |> List.hd

let first lines =
  let cycle = lines |> List.map nodes_of_line |> get_cycle in
  let map = Array.init 140 (fun _ -> Array.init 140 (fun _ -> "*")) in
  List.iter (fun (a, b, symbol) -> map.(a).(b) <- symbol) cycle;
  map.(52).(100) <- "S";
  Array.iter
    (fun l ->
      print_endline @@ String.concat "" @@ List.of_seq @@ Array.to_seq l)
    map;
  List.length cycle / 2

let second lines = List.length lines
