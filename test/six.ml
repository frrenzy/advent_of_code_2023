let () =
  let input = Advent.File.read_lines_test 6 in

  let first = Advent.Six.first input in
  let second = Advent.Six.second input in

  Advent.Test.assert' first 288 "6.1 test invalid";
  Advent.Test.assert' second 71503 "6.2 test invalid";

  let input = Advent.File.read_lines 6 in

  let first = Advent.Six.first input in
  let second = Advent.Six.second input in

  Advent.Test.assert' first 608902 "6.1 real invalid";
  Advent.Test.assert' second 46173809 "6.2 real invalid";

  print_endline "6 good"
