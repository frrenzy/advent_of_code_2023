let tokens_of_line line = Utils.split " " line |> List.map int_of_string

let fold numbers =
  let rec aux values acc =
    if List.for_all (( = ) 0) values then acc @ [ Lists.last numbers ]
    else
      let round =
        List.tl
        @@ List.mapi
             (fun i x ->
               match i with 0 -> 0 | i' -> x - (List.nth values @@ pred i'))
             values
      in
      aux round (Lists.last round :: acc)
  in
  List.rev @@ aux numbers []

let process rev lines =
  let numbers = List.map tokens_of_line lines in
  let values = if rev = true then List.map List.rev numbers else numbers in
  List.map fold values |> List.map Lists.sum |> Lists.sum

let first = process false
let second = process true
