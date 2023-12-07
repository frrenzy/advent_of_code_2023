let test_five () =
  let input = Advent.File.read_lines_test 5 in
  let first = Advent.Five.first input in
  let second = Advent.Five.second input in
  Advent.Test.assert' (first = 35) "5.1 invalid";
  Advent.Test.assert' (second = 46) "5.2 invalid";
  print_endline "5 good"

let test_six () =
  let input = Advent.File.read_lines_test 6 in
  let first = Advent.Six.first input in
  let second = Advent.Six.second input in
  Advent.Test.assert' (first = 288) "6.1 invalid";
  Advent.Test.assert' (second = 71503) "6.2 invalid";
  print_endline "6 good"

let test_seven () =
  let input = Advent.File.read_lines_test 7 in
  let first = Advent.Seven.first input in
  let second = Advent.Seven.second input in
  Advent.Test.assert' (first = 6440) "7.1 test invalid";
  Advent.Test.assert' (second = 5905) "7.2 test invalid";
  let input = Advent.File.read_lines 7 in
  let first = Advent.Seven.first input in
  let second = Advent.Seven.second input in
  Advent.Test.assert' (first = 251927063) "7.1 real invalid";
  Advent.Test.assert' (second = 255632664) "7.2 real invalid";
  print_endline "7 good"

let () =
  (* test_five (); *)
  (* test_six (); *)
  test_seven ()
