let () =
  let input = Advent.File.read_lines_test 19 in

  let first = Advent.Nineteen.first input in
  let second = Advent.Nineteen.second input in

  Advent.Test.assert' first 19114 "19.1 test invalid";
  Advent.Test.assert' second 167409079868000 "19.2 test invalid";

  let input = Advent.File.read_lines 19 in

  let first = Advent.Nineteen.first input in
  let second = Advent.Nineteen.second input in

  Advent.Test.assert' first 480738 "19.1 real invalid";
  Advent.Test.assert' second 255632664 "19.2 real invalid";

  print_endline "19 good"

