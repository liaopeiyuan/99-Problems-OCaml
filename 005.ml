let rev l = 
  let rec help l' = function
   | [] -> []
   | x::xs -> help (x::l') xs in
  help [] l;;
