let () =
  let lines = Advent.File.read_lines 19 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Nineteen.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.Nineteen.second lines
