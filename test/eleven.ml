let () =
  let input = Advent.File.read_lines_test 11 in

  let first = Advent.Eleven.first input in

  let second = Advent.Eleven.second input in

  Advent.Test.assert' first 374 "11.1 test invalid";
  Advent.Test.assert' second 82000210 "11.2 test invalid";

  let input = Advent.File.read_lines 11 in

  let first = Advent.Eleven.first input in
  let second = Advent.Eleven.second input in

  Advent.Test.assert' first 10228230 "11.1 real invalid";
  Advent.Test.assert' second 447073334102 "11.2 real invalid";
  print_endline "11 good"
