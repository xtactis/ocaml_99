(* 97 sudoku solver *)
exception Failure of string

let findi f l = 
  let rec aux i = function
    | [] -> raise (Failure "Not found")
    | h :: t -> if f i h then i else aux (i+1) t in
  aux 0 l

module Board = struct
  let rec from_flat_list l = 
    let rec aux acc i = function
      | [] -> acc
      | h :: t -> if i != 9 then aux (h :: acc) (i+1) t else (h :: acc) in
    let rec aux2 acc i = function
      | [] -> acc
      | h :: t -> if i mod 9 == 0 then aux2 ((List.rev (aux [] 1 (h :: t))) :: acc) (i+1) t else aux2 acc (i+1) t in
    List.rev (aux2 [] 0 l)
  let get_row board k = List.nth board k
  let get_col board k = List.fold_left (fun acc x -> (List.nth x k) :: acc) [] board
  let get_area board k = 
    let row = k / 3 * 3 in
    let col = (k mod 3) * 3 in
    let rec aux acc i = function
      | [] -> acc
      | h :: t -> if i >= col && i <= (col+2) then aux (h :: acc) (i+1) t else aux acc (i+1) t in
    List.rev (aux (aux (aux [] 0 (get_row board row)) 0 (get_row board (row+1))) 0 (get_row board (row+2))) (* yuck *)
  let print board = 
    let print_row = 
      List.iteri (fun i x -> Printf.printf "%d %s" x (if i mod 3 = 2 && i != 8 then "| " else "")) in
    (List.iteri (fun i x -> print_row x;
                 Printf.printf "\n";
                 if i mod 3 = 2 && i != 8 then Printf.printf "%s\n" (String.make 21 '-')) board;
    Printf.printf "\n")
end

let not_missing l = 
  let res = List.sort_uniq compare l in 
  match res with
    | 0 :: t -> List.tl res
    | _ -> res

let missing l =
  let rec aux acc i = function
    | [] -> if i < 10 then aux (i :: acc) (i+1) [] else acc
    | h :: t -> if h = i then aux acc (i+1) t else aux (i :: acc) (i+1) (h :: t) in
  List.rev (aux [] 1 (not_missing l));;

let count_zero = List.fold_left (fun acc x -> if x = 0 then acc + 1 else acc) 0;;

let change_nth n l x = 
  let rec aux acc i = function
    | [] -> acc
    | h :: t -> aux ((if i = n then x else h) :: acc) (i+1) t in
  List.rev (aux [] 0 l)

let sudoku board =
  let moves = count_zero (List.flatten board) in
  let rec play cur i = 
    let flat_cur = List.flatten cur in
    let get_clues i =
      let row = i / 9 in
      let col = i mod 9 in 
      let area = row/3*3+col/3 in
      List.concat [(Board.get_row cur row); (Board.get_col cur col); (Board.get_area cur area)] in
    let check_square i x = 
      let count = List.length (not_missing (get_clues i)) in 
      x == 0 && count = 8 in
    let play_move i =
      let new_value = List.hd (missing (get_clues i)) in 
      Board.from_flat_list (change_nth i flat_cur new_value) in
    if i = moves then cur
    else play (play_move (findi check_square flat_cur)) (i+1) in
  play board 0
      

let () = 
  let initial_board = [[0; 0; 4;  8; 0; 0;  0; 1; 7];
                      [6; 7; 0;  9; 0; 0;  0; 0; 0];
                      [5; 0; 8;  0; 3; 0;  0; 0; 4];
                      [3; 0; 0;  7; 4; 0;  1; 0; 0];
                      [0; 6; 9;  0; 0; 0;  7; 8; 0];
                      [0; 0; 1;  0; 6; 9;  0; 0; 5];
                      [1; 0; 0;  0; 8; 0;  3; 0; 6];
                      [0; 0; 0;  0; 0; 6;  0; 9; 1];
                      [2; 4; 0;  0; 0; 1;  5; 0; 0]] in
  (*Board.print initial_board;*)
  Board.print (sudoku initial_board);