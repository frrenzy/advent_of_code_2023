let assert' e message = try assert e with Assert_failure _ -> failwith message
