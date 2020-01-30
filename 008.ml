let rec compress = function
    | [] -> []
    | [x] -> [x]
    | x::y::xs -> if x=y then compress (y::xs) else x::compress(y::xs)
