let rec replicate a n =
    let rec rep e n = match n with 
        | 0 -> []
        | n -> e::(rep e (n-1))
    in
    match (a,n) with
        | (_, 0) -> []
        | ([], n) -> []
        | (x::xs, n) -> (rep x n)@(replicate xs n)
