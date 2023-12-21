type part = { x : int; m : int; a : int; s : int }

type rule = {
  field : char;
  func : int -> int -> bool;
  value : int;
  dest : string;
}

type process = Pass of string | Examine of rule

module Flows = Map.Make (String)

exception Accepted
exception Rejected

let is_passing { x; m; a; s } = function
  | Pass _ -> true
  | Examine { field; func; value; _ } -> (
      match field with
      | 'x' -> func x value
      | 'm' -> func m value
      | 'a' -> func a value
      | 's' -> func s value
      | _ -> failwith "bad field")

let next_wf = function
  | Pass "A" | Examine { dest = "A"; _ } -> raise Accepted
  | Pass "R" | Examine { dest = "R"; _ } -> raise Rejected
  | Pass dest | Examine { dest; _ } -> dest

let parse_cmd cmd =
  let (rule :: dest) = Utils.split ":" cmd in
  if dest = [] then Pass rule
  else
    let field = rule.[0] in
    let len = String.length rule - 2 in
    let dest = List.hd dest in
    let value = int_of_string @@ String.sub rule 2 len in
    match rule.[1] with
    | '<' -> Examine { field; func = ( < ); value; dest }
    | '>' -> Examine { field; func = ( > ); value; dest }
    | _ -> failwith "bad cmd"

let parse_command map line =
  let [ name; cmds ] = Utils.split "{" line in
  let cmds = String.sub cmds 0 @@ (String.length cmds - 1) in
  let cmds' = Utils.split "," cmds |> List.map parse_cmd in
  Flows.add name cmds' map

let parse_part part =
  let len = String.length part - 2 in
  let tokens =
    String.sub part 1 len |> Utils.split ","
    |> List.map (Utils.split "=")
    |> List.map (fun [ _; v ] -> int_of_string v)
  in
  {
    x = List.nth tokens 0;
    m = List.nth tokens 1;
    a = List.nth tokens 2;
    s = List.nth tokens 3;
  }

let parse lines =
  let (Some split) = List.find_index (( = ) "") lines in
  let commands, _ :: parts =
    List.mapi (fun i l -> (i, l)) lines
    |> List.partition_map (fun (i, l) -> if i < split then Left l else Right l)
  in
  (List.fold_left parse_command Flows.empty commands, List.map parse_part parts)

let process commands part =
  let rec aux name =
    let rules = Flows.find name commands in
    let rule = List.find (is_passing part) rules in
    aux (next_wf rule)
  in
  try aux "in" with
  | Rejected -> 0
  | Accepted -> part.x + part.m + part.a + part.s

let first lines =
  let commands, parts = parse lines in
  List.map (process commands) parts |> Lists.sum

let second lines =
  let commands, _ = parse lines in
  (* print_endline @@ string_of_int @@ Flows.cardinal commands; *)
  0
