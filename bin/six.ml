let () =
  let lines = Advent.File.read_lines 6 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Six.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.Six.second lines
