let () =
  let input = Advent.File.read_lines_test 16 in

  let first = Advent.Sixteen.first input in
  let second = Advent.Sixteen.second input in

  Advent.Test.assert' first 46 "16.1 test invalid";
  Advent.Test.assert' second 51 "16.2 test invalid";

  let input = Advent.File.read_lines 16 in

  let first = Advent.Sixteen.first input in
  let second = Advent.Sixteen.second input in

  Advent.Test.assert' first 6883 "16.1 real invalid";
  Advent.Test.assert' second 7228 "16.2 real invalid";

  print_endline "16 good"

