type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let rec decode = function
    | [] -> []
    | (One x) :: xs -> x :: (decode xs)
    | (Many (t, x)) :: xs -> if t = 1 then x :: (decode xs) else x :: (decode (Many ((t-1),x)::xs))
