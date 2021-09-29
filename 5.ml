let rev l = 
  let rec aux acc = 
    | [] -> acc
    | h :: t -> aux (h :: acc) t in
  aux [] l