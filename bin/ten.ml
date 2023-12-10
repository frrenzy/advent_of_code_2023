let () =
  let lines = Advent.File.read_lines 10 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Ten.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.Ten.second lines
