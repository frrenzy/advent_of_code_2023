let () =
  let input = Advent.File.read_lines_test 4 in

  let first = Advent.Four.first input in
  let second = Advent.Four.second input in

  Advent.Test.assert' first 13 "4.1 test invalid";
  Advent.Test.assert' second 30 "4.2 test invalid";

  let input = Advent.File.read_lines 4 in

  let first = Advent.Four.first input in
  let second = Advent.Four.second input in

  Advent.Test.assert' first 26443 "4.1 real invalid";
  Advent.Test.assert' second 6284877 "4.2 real invalid";

  print_endline "4 good"
