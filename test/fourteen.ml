let () =
  let input = Advent.File.read_lines_test 14 in

  let first = Advent.Fourteen.first input in

  (* let second = Advent.Fourteen.second input in *)
  Advent.Test.assert' first 136 "14.1 test invalid";

  (* Advent.Test.assert' second 5905 "14.2 test invalid"; *)
  let input = Advent.File.read_lines 14 in

  let first = Advent.Fourteen.first input in

  (* let second = Advent.Fourteen.second input in *)
  Advent.Test.assert' first 108144 "14.1 real invalid";
  (* Advent.Test.assert' second 255632664 "14.2 real invalid"; *)
  print_endline "14 good"

