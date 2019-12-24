let rec last_two = function
    [] -> None
  | [_] -> None
  | x :: y :: [] -> Some (x,y)
  | x :: xs -> last_two xs;;
