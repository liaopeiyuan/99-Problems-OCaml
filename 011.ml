type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode x =
    let rec encode' a b = match (a,b) with
          | ([], e) -> e
          | ((y::ys),[]) -> encode' ys ([One y])
          | ((y::ys),((One e)::xs)) -> if e = y then encode' ys ((Many (2,e))::xs)
                                              else encode' ys ((One y)::((One e)::xs))
          | ((y::ys),((Many (n,e))::xs)) -> if e = y then encode' ys ((Many (n+1,e))::xs)
                                              else encode' ys ((One y)::((Many (n,e))::xs))
                                              
    in
    List.rev (encode' x [])

