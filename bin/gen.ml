module NMap = Map.Make (Int)

let map =
  NMap.(
    empty |> add 1 "one" |> add 2 "two" |> add 3 "three" |> add 4 "four"
    |> add 5 "five" |> add 6 "six" |> add 7 "seven" |> add 8 "eight"
    |> add 9 "nine" |> add 10 "ten" |> add 11 "eleven" |> add 12 "twelve"
    |> add 13 "thirteen" |> add 14 "fourteen" |> add 15 "fifteen"
    |> add 16 "sixteen" |> add 17 "seventeen" |> add 18 "eighteen"
    |> add 19 "nineteen" |> add 20 "twenty" |> add 21 "twentyOne"
    |> add 22 "twentyTwo" |> add 23 "twentyThree" |> add 24 "twentyFour"
    |> add 25 "twentyFive")

let bin_name number = "./bin/" ^ NMap.find number map ^ ".ml"
let module_name number = "./lib/" ^ NMap.find number map ^ ".ml"
let interface_name number = module_name number ^ "i"

let write_new_files number =
  let bin_file_name = bin_name number in
  let bin_chan = open_out bin_file_name in
  let module_internal_name = String.capitalize_ascii @@ NMap.find number map in
  Printf.fprintf bin_chan
    "let () =\n\
    \  let lines = Advent.File.read_lines %d in\n\
    \  print_endline %@%@ \"first: \" ^ string_of_int %@%@ Advent.%s.first \
     lines;\n\
    \  print_endline %@%@ \"second: \" ^ string_of_int %@%@ Advent.%s.second \
     lines\n"
    number module_internal_name module_internal_name;
  close_out bin_chan;

  let interface_file_name = interface_name number in
  let module_chan = open_out interface_file_name in
  Printf.fprintf module_chan
    "val first : string list -> int\nval second : string list -> int\n";
  close_out module_chan;

  let module_file_name = module_name number in
  let module_chan = open_out module_file_name in
  Printf.fprintf module_chan
    "let first lines = List.length lines\n\
     let second lines = List.length lines\n";
  close_out module_chan

let lib_dune = "./lib/dune"
let bin_dune = "./bin/dune"

let transform_line line content =
  String.sub line 0 (String.length line - 1) ^ " " ^ content ^ ")"

let append_files number =
  let lib_content = Advent.File.read lib_dune in
  let lib_chan = open_out lib_dune in
  let print line =
    let prefix = "  (modules" in
    match String.starts_with ~prefix line with
    | true ->
        Printf.fprintf lib_chan "%s\n"
        @@ transform_line line @@ NMap.find number map
    | false -> Printf.fprintf lib_chan "%s\n" line
  in
  List.iter print lib_content;

  let bin_content = Advent.File.read bin_dune in
  let bin_chan = open_out bin_dune in
  let print line =
    match
      String.starts_with ~prefix:"  (names" line
      || String.starts_with ~prefix:"  (public_names" line
    with
    | true ->
        Printf.fprintf bin_chan "%s\n"
        @@ transform_line line @@ NMap.find number map
    | false -> Printf.fprintf bin_chan "%s\n" line
  in
  List.iter print bin_content

let write_makefile number =
  let chan = open_out_gen [ Open_append; Open_creat ] 0o666 "Makefile" in
  Printf.fprintf chan "\n%d:\n\tdune exec %s\n" number @@ NMap.find number map;
  close_out chan

let main number =
  ignore @@ Sys.command @@ "get_advent 2023 " ^ string_of_int number;
  append_files number;
  write_new_files number;
  write_makefile number

let () =
  match Array.length Sys.argv with
  | 1 -> failwith "no number given"
  | 2 -> (
      match int_of_string_opt Sys.argv.(1) with
      | Some number -> main number
      | None -> failwith "NaN")
  | _ -> failwith "too much"
