let () =
  let lines = Advent.File.read_lines 16 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Sixteen.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.Sixteen.second lines
