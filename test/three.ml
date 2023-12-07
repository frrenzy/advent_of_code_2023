let () =
  let input = Advent.File.read_lines_test 3 in

  let first = Advent.Three.first input in
  let second = Advent.Three.second input in

  Advent.Test.assert' first 4361 "3.1 test invalid";
  Advent.Test.assert' second 467835 "3.2 test invalid";

  let input = Advent.File.read_lines 3 in

  let first = Advent.Three.first input in
  let second = Advent.Three.second input in

  Advent.Test.assert' first 527364 "3.1 real invalid";
  Advent.Test.assert' second 79026871 "3.2 real invalid";

  print_endline "3 good"
