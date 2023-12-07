let assert' a e message =
  try assert (a = e)
  with Assert_failure _ ->
    failwith
      (message ^ ": Expected " ^ string_of_int e ^ ", Got " ^ string_of_int a)
