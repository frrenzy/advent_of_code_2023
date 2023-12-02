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
