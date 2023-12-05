let () =
  let lines = Advent.File.read_lines 5 in
  print_endline @@ "first: " ^ string_of_int @@ Advent.Five.first lines;
  print_endline @@ "second: " ^ string_of_int @@ Advent.Five.second lines
