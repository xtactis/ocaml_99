let rec last = function
  | [] -> None
  | [x] -> Some h
  | _ :: t -> last t;;