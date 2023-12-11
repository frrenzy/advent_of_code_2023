let int c = int_of_char c - int_of_char '0'

let file_name number is_test =
  let str_num = string_of_int number in
  "inputs/" ^ str_num ^ (if is_test then "_test" else "") ^ ".txt"

let split pattern str =
  let re = Str.regexp_string pattern in
  Str.split re str

let rec gcd a = function 0 -> a | b -> gcd b (a mod b)

let lcm a b =
  let d = gcd a b in
  a * (b / d)

let half a = a / 2
let double = ( * ) 2
