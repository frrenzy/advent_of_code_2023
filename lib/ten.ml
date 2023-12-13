type dir = Top | Bottom | Left | Right | Invalid_dir | Any_dir
type pipe = Pipe of dir * dir | Ground | Start | Any_pipe

exception Invalid_direction

class node symbol =
  object
    val t =
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

    val v = match symbol with 'L' | 'J' | '7' | 'S' | 'F' -> true | _ -> false
    method is_vertex = v
    method symbol = symbol

    method next_dir (from : dir) =
      match (from, t) with
      | _, Ground | _, Any_pipe -> Invalid_dir
      | _, Start -> Any_dir
      | from, Pipe (a, b) -> (
          match from with
          | f when f = a -> b
          | f when f = b -> a
          | _ -> Invalid_dir)
  end

let starting grid =
  let grid = List.map (List.map (fun n -> n#symbol)) grid in
  let a = List.find_index (List.mem 'S') grid |> Option.get in
  let b = List.find_index (( = ) 'S') (List.nth grid a) |> Option.get in
  [ (a - 1, b, Bottom); (a + 1, b, Top); (a, b - 1, Right); (a, b + 1, Left) ]

let nodes_of_line l = String.to_seq l |> List.of_seq |> List.map (new node)
let get (a, b) grid = List.nth (List.nth grid a) b
let half a = a / 2

let get_cycle (grid : node list list) =
  let rec aux (a, b) from pos acc =
    let node =
      try get (a, b) grid with Failure _ -> raise Invalid_direction
    in
    match node#next_dir from with
    | Top -> aux (a - 1, b) Bottom (pos + 1) ((a, b, node#is_vertex) :: acc)
    | Bottom -> aux (a + 1, b) Top (pos + 1) ((a, b, node#is_vertex) :: acc)
    | Right -> aux (a, b + 1) Left (pos + 1) ((a, b, node#is_vertex) :: acc)
    | Left -> aux (a, b - 1) Right (pos + 1) ((a, b, node#is_vertex) :: acc)
    | Any_dir -> (a, b, node#is_vertex) :: acc
    | _ -> raise @@ Invalid_direction
  in
  List.filter_map
    (fun (a, b, from) ->
      try Some (aux (a, b) from 2 []) with Invalid_direction -> None)
    (starting grid)
  |> List.hd

let shoelace cycle =
  let vertices = List.filter (fun (_, _, is_vertex) -> is_vertex) cycle in
  let len = List.length vertices in
  List.mapi
    (fun i (a1, b1, _) ->
      let a2, b2, _ = List.nth vertices ((i + 1) mod len) in
      (a1 * b2) - (a2 * b1))
    vertices
  |> Lists.sum |> abs |> half

let pick cycle = 1 - (List.length cycle / 2) + shoelace cycle
let process f lines = List.map nodes_of_line lines |> get_cycle |> f
let first = process (fun l -> List.length l / 2)
let second = process pick
