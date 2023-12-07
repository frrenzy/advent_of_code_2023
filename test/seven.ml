let () =
  let input = Advent.File.read_lines_test 7 in

  let first = Advent.Seven.first input in
  let second = Advent.Seven.second input in

  Advent.Test.assert' first 6440 "7.1 test invalid";
  Advent.Test.assert' second 5905 "7.2 test invalid";

  let input = Advent.File.read_lines 7 in

  let first = Advent.Seven.first input in
  let second = Advent.Seven.second input in

  Advent.Test.assert' first 251927063 "7.1 real invalid";
  Advent.Test.assert' second 255632664 "7.2 real invalid";

  print_endline "7 good"
