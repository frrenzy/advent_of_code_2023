let () =
  let lines = Advent.File.read_lines 7 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Seven.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.Seven.second lines
