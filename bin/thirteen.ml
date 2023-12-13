let () =
  let lines = Advent.File.read_lines 13 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Thirteen.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.Thirteen.second lines
