let int c = int_of_char c - int_of_char '0'

let file_name number =
  let str_num = string_of_int number in
  "inputs/" ^ str_num ^ ".txt"

let split pattern str =
  let re = Str.regexp_string pattern in
  Str.split re str
