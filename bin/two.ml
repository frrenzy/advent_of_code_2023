let () =
  let lines = Advent.File.read_lines 2 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Two.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.Two.second lines
