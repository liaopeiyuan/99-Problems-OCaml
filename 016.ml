let rec drop l n = match (l,n) with | (_, 0) -> l | ([], _) -> [] | (x::xs, 1) -> xs | (x::xs, n) -> x::(drop xs (n-1))
