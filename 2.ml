let rec last_two = function
  | [] | [x] -> None
  | [x; y] -> Some (x, y)
  | _ :: t -> last_two t;;