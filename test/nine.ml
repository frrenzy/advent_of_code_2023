let () =
  let input = Advent.File.read_lines_test 9 in

  let first = Advent.Nine.first input in
  let second = Advent.Nine.second input in

  Advent.Test.assert' first 114 "9.1 test invalid";
  Advent.Test.assert' second 2 "9.2 test invalid";

  let input = Advent.File.read_lines 9 in

  let first = Advent.Nine.first input in
  let second = Advent.Nine.second input in

  Advent.Test.assert' first 1641934234 "9.1 real invalid";
  Advent.Test.assert' second 975 "9.2 real invalid";
  print_endline "9 good"
