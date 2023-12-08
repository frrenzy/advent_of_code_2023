let () =
  let lines = Advent.File.read_lines 8 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Eight.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.Eight.second lines
