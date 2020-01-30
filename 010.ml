 
let encode x =
    let rec encode' a b = match (a,b) with
          | ([], e) -> e
          | ((y::ys),[]) -> encode' ys ([(1,y)])
          | ((y::ys),((n,e)::xs)) -> if e = y then encode' ys ((n+1,e)::xs)
                                              else encode' ys ((1,y)::((n,e)::xs))
    in
    List.rev (encode' x [])

