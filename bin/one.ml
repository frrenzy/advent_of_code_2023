let () =
  let lines = Advent.File.read_lines 1 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.One.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.One.second lines
