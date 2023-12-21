let () =
  let input = Advent.File.read_lines_test 18 in

  let first = Advent.Eighteen.first input in
  let second = Advent.Eighteen.second input in

  Advent.Test.assert' first 62 "18.1 test invalid";
  Advent.Test.assert' second 952408144115 "18.2 test invalid";

  let input = Advent.File.read_lines 18 in

  let first = Advent.Eighteen.first input in
  let second = Advent.Eighteen.second input in

  Advent.Test.assert' first 49061 "18.1 real invalid";
  Advent.Test.assert' second 92556825427032 "18.2 real invalid";

  print_endline "18 good"

