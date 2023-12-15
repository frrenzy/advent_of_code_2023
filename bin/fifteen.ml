let () =
  let lines = Advent.File.read_lines 15 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Fifteen.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.Fifteen.second lines
