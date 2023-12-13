let () =
  let input = Advent.File.read_lines_test 13 in

  let first = Advent.Thirteen.first input in
  let second = Advent.Thirteen.second input in

  Advent.Test.assert' first 405 "13.1 test invalid";
  Advent.Test.assert' second 400 "13.2 test invalid";

  let input = Advent.File.read_lines 13 in

  let first = Advent.Thirteen.first input in
  let second = Advent.Thirteen.second input in

  Advent.Test.assert' first 37975 "13.1 real invalid";
  Advent.Test.assert' second 32497 "13.2 real invalid";

  print_endline "13 good"

