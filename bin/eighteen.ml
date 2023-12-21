let () =
  let lines = Advent.File.read_lines 18 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Eighteen.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.Eighteen.second lines
