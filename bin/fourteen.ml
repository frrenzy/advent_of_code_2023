let () =
  let lines = Advent.File.read_lines 14 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Fourteen.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.Fourteen.second lines
