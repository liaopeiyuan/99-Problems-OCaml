exception Impossible

let pack x = 
    let rec pack' a b = match (a,b) with
          | ([], e) -> e
          | ((y::ys),[]) -> pack' ys ([[y]])
          | ((y::ys),((e::es)::xs)) -> if e = y then pack' ys ((y::e::es)::xs)
                                                   else pack' ys ([y]::((e::es)::xs))
          | _ -> raise Impossible
    in
    List.rev (pack' x [])
