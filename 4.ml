let rec length = function
  | [] -> 0
  | h :: t -> 1+(length t);;