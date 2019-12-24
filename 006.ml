let is_palindrome l =
    let rec help l1 l2 = match (l1, l2) with 
      | ([], []) -> true
      | (x::_, []) -> false
      | ([], x::_) -> false
      | (x::xs, y::ys) -> if x = y then help xs ys else false
    in
    help l (List.rev l);;

