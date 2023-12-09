let () =
  let lines = Advent.File.read_lines 9 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Nine.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.Nine.second lines
