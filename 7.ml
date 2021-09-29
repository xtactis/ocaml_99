type 'a node =
  | One of 'a
  | Many of 'a node list;;

let flatten l =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many x :: t -> aux (aux acc x) t in
  List.rev (aux [] l);;