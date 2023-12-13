let rec last = function
  | [] -> failwith "empty"
  | [ x ] -> x
  | _ :: xs -> last xs

let rec print_char_list = function
  | [] -> print_newline ()
  | e :: l ->
      print_char e;
      print_string "; ";
      print_char_list l

let rec print_int_list = function
  | [] -> print_newline ()
  | e :: l ->
      print_int e;
      print_string " ";
      print_int_list l

let print_pair p =
  print_int @@ fst p;
  print_string " ";
  print_int @@ snd p;
  print_newline ()

let rec get ls index =
  match index with
  | 0 -> List.hd ls
  | a when a > 0 -> get (List.tl ls) (a - 1)
  | _ -> failwith "negative index"

let range a b =
  let rec aux acc a = function
    | 0 -> acc
    | b -> aux (a :: acc) (a + 1) (b - 1)
  in
  List.rev @@ aux [] a b

let to_pairs l =
  l
  |> List.mapi (fun i e ->
         match i mod 2 with 0 -> Some (e, List.nth l (i + 1)) | _ -> None)
  |> List.filter Option.is_some |> List.map Option.get

let sum = List.fold_left ( + ) 0
let product = List.fold_left ( * ) 0
let print l sep = List.map string_of_int l |> String.concat sep |> print_endline

let rec slice_from l pos =
  match (l, pos) with
  | l, 0 -> l
  | _ :: xs, a -> slice_from xs (a - 1)
  | [], _ -> raise @@ Invalid_argument "index too high"

let slice_to l pos =
  let rec aux l' pos' acc =
    match (l', pos') with
    | _, 0 -> acc
    | x :: xs, a -> aux xs (a - 1) (x :: acc)
    | [], _ ->
        raise
        @@ Invalid_argument
             ("index too high: " ^ string_of_int pos ^ ", where len "
            ^ string_of_int @@ List.length l)
  in
  List.rev @@ aux l pos []

let slice l from len =
  let from_slice = slice_from l from in
  slice_to from_slice len

let insert l pos el =
  let left = slice_to l pos in
  let right = slice_from l pos in
  left @ (el :: right)

let transpose m =
  List.init
    (List.length @@ List.hd m)
    (fun i -> List.map (fun l -> List.nth l i) m)
