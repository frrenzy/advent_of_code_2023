let () =
  let lines = Advent.File.read_lines 3 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Three.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.Three.second lines
