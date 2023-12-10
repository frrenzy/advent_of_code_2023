let () =
  (* let input = Advent.File.read_lines_test 10 in *)

  (* let first = Advent.Ten.first input in *)

  (* let second = Advent.Ten.second input in *)
  (* Advent.Test.assert' first (-1) "10.1 test invalid"; *)

  (* Advent.Test.assert' second 5 "10.2 test invalid"; *)
  let input = Advent.File.read_lines 10 in

  let first = Advent.Ten.first input in

  (* let second = Advent.Ten.second input in *)
  Advent.Test.assert' first 6823 "10.1 real invalid";
  (* Advent.Test.assert' second 140 "10.2 real invalid"; *)
  print_endline "10 good"
