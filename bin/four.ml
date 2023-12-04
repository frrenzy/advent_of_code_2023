let () =
  let lines = Advent.File.read_lines 4 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Four.first lines;
  let lines = Advent.File.read_lines 4 in
  print_endline @@ "second: " ^ string_of_int @@ Advent.Four.second lines
