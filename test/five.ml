let () =
  let input = Advent.File.read_lines_test 5 in

  let first = Advent.Five.first input in
  let second = Advent.Five.second input in

  Advent.Test.assert' first 35 "5.1 test invalid";
  Advent.Test.assert' second 46 "5.2 test invalid";

  let input = Advent.File.read_lines 5 in

  let first = Advent.Five.first input in
  let second = Advent.Five.second input in

  Advent.Test.assert' first 389056265 "5.1 real invalid";
  Advent.Test.assert' second 137516820 "5.2 real invalid";

  print_endline "5 good"
