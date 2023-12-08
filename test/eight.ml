let () =
  let input = Advent.File.read_lines_test 8 in

  let first = Advent.Eight.first input in
  let second =
    Advent.Eight.second
      [
        "LR";
        "";
        "11A = (11B, XXX)";
        "11B = (XXX, 11Z)";
        "11Z = (11B, XXX)";
        "22A = (22B, XXX)";
        "22B = (22C, 22C)";
        "22C = (22Z, 22Z)";
        "22Z = (22B, 22B)";
        "XXX = (XXX, XXX)";
      ]
  in

  Advent.Test.assert' first 6 "8.1 test invalid";
  Advent.Test.assert' second 6 "8.2 test invalid";

  let input = Advent.File.read_lines 8 in

  let first = Advent.Eight.first input in
  let second = Advent.Eight.second input in

  Advent.Test.assert' first 23147 "8.1 real invalid";
  Advent.Test.assert' second 22289513667691 "8.2 real invalid";

  print_endline "8 good"
