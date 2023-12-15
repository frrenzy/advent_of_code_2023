let () =
  let input = Advent.File.read_lines_test 15 in

  let first = Advent.Fifteen.first input in
  let second = Advent.Fifteen.second input in

  Advent.Test.assert' first 1320 "15.1 test invalid";
  Advent.Test.assert' second 145 "15.2 test invalid";

  let input = Advent.File.read_lines 15 in

  let first = Advent.Fifteen.first input in
  let second = Advent.Fifteen.second input in

  Advent.Test.assert' first 510273 "15.1 real invalid";
  Advent.Test.assert' second 212449 "15.2 real invalid";
  print_endline "15 good"

