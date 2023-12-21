let int c =
  let r = int_of_char c - int_of_char '0' in
  if r < 10 then r else r - 7

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

let shoelace vertices =
  let len = List.length vertices in
  List.mapi
    (fun i (a1, b1) ->
      let a2, b2 = List.nth vertices ((i + 1) mod len) in
      (a1 * b2) - (a2 * b1))
    vertices
  |> Lists.sum |> abs |> half

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let int_of_hex s =
  let rec aux acc power = function
    | "" -> acc
    | s' ->
        let last = String.length s' - 1 in
        aux
          ((int s'.[last] * pow 16 power) + acc)
          (power + 1) (String.sub s' 0 last)
  in

  aux 0 0 @@ String.uppercase_ascii s

let half a = a / 2
let double = ( * ) 2
