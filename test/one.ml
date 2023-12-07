let () =
  let input = Advent.File.read_lines_test 1 in

  let first = Advent.One.first input in
  let second = Advent.One.second input in

  Advent.Test.assert' first 142 "1.1 test invalid";
  Advent.Test.assert' second 142 "1.2 test invalid";

  let input = Advent.File.read_lines 1 in

  let first = Advent.One.first input in
  let second = Advent.One.second input in

  Advent.Test.assert' first 55607 "1.1 real invalid";
  Advent.Test.assert' second 55291 "1.2 real invalid";

  print_endline "1 good"
