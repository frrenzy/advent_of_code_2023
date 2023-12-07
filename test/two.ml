let () =
  let input = Advent.File.read_lines_test 2 in

  let first = Advent.Two.first input in
  let second = Advent.Two.second input in

  Advent.Test.assert' first 8 "2.1 test invalid";
  Advent.Test.assert' second 2286 "2.2 test invalid";

  let input = Advent.File.read_lines 2 in

  let first = Advent.Two.first input in
  let second = Advent.Two.second input in

  Advent.Test.assert' first 2439 "2.1 real invalid";
  Advent.Test.assert' second 63711 "2.2 real invalid";

  print_endline "2 good"
