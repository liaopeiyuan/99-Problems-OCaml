type 'a node =
    | One of 'a
    | Many of 'a node list;;

let flatten (l : 'a node list) = 
    let rec help list s = match list with
      | [] -> s []
      | (One x) :: xs -> help xs (fun e -> s(x::e))
      | (Many l') :: xs -> help xs (fun e -> help l' (fun e2 -> s(e2@e)))
    in
    help l (fun x -> x) 
