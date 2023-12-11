let () =
  let lines = Advent.File.read_lines 11 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Eleven.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.Eleven.second lines
