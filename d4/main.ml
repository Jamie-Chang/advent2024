let read_matrix filename =
  In_channel.with_open_text filename In_channel.input_lines
  |> List.map (fun l -> List.of_seq (String.to_seq l))

let window n lst = 
  let rec take n lst = 
    if n = 0 then [] else
    match lst with 
      | [] -> failwith "not expected"
      | x :: xs -> x::(take (n - 1) xs) in 
  let rec window_rec lst () = 
    if List.length lst < n then Seq.Nil
    else Seq.Cons (take n lst, window_rec (List.tl lst)) in
  window_rec lst


let read_verticle matrix =
  let split_rows matrix =
    Seq.map
      (fun row -> match row with a :: other -> (a, other) | [] -> failwith "")
      (List.to_seq matrix)
  in
  let read_left matrix =
    Seq.fold_left
      (fun (left_col, rest_cols) (left, rest) ->
        (left :: left_col, rest :: rest_cols))
      ([], []) (split_rows matrix)
  in
  let is_empty matrix = List.for_all (fun a -> a = []) matrix in
  let rec read_verticle_rec columns matrix =
    if is_empty matrix then columns
    else
      let column, rest = read_left matrix in
      read_verticle_rec (column :: columns) rest
  in

  read_verticle_rec [] matrix

let read_diagonal matrix =
  let rec read_diagonal_rec diagonals rows rest =
    let split_rows matrix =
      List.to_seq matrix
      |> Seq.map (fun row ->
             match row with a :: other -> (a, other) | [] -> failwith "")
    in
    let read_left matrix =
      Seq.fold_left
        (fun (left_col, rest_cols) (left, rest) ->
          (left :: left_col, rest :: rest_cols))
        ([], []) (split_rows matrix)
    in
    let rows = List.filter (fun l -> l <> []) rows in
    if rows <> [] then
      let left, rows_rev = read_left rows in
      match rest with
      | [] -> read_diagonal_rec (left :: diagonals) (List.rev rows_rev) rest
      | next_row :: rest ->
          read_diagonal_rec (left :: diagonals)
            (List.rev (next_row :: rows_rev))
            rest
    else diagonals
  in
  match matrix with
  | first :: rest -> read_diagonal_rec [] [first] rest
  | _ -> failwith "expected at least one row"
  

let count_word line =
  let rec count_word_rec count line =
    match line with
    | 'X' :: 'M' :: 'A' :: 'S' :: rest ->
        count_word_rec (count + 1) ('M' :: 'A' :: 'S' :: rest)
    | 'S' :: 'A' :: 'M' :: 'X' :: rest ->
        count_word_rec (count + 1) ('A' :: 'M' :: 'X' :: rest)
    | _ :: rest -> count_word_rec count rest
    | [] -> count
  in
  count_word_rec 0 line

let sum_seq seq = Seq.fold_left (fun acc v -> acc + v) 0 seq

let part1 filename =
  let matrix = read_matrix filename in
  let horizontal = matrix in
  let verticle = read_verticle matrix in
  let d1 = read_diagonal matrix in
  let d2 = read_diagonal (List.rev matrix) in
  let count_all lines = List.to_seq lines |> Seq.map count_word |> sum_seq in
  count_all horizontal + count_all verticle + count_all d1 + count_all d2
;;

part1 "inputs/d4.txt" |> string_of_int |> print_endline

let mas = ['M' ; 'A' ; 'S']
let sam = ['S' ; 'A' ; 'M']

let find_cross_horizontal = 
  let rec find_cross_horizontal_rec count matrix = 
    match matrix with
    |  (c00 :: c01 :: c02 :: r03) 
    :: (_  :: c11 :: c12 :: r13) 
    :: (c20 :: c21 :: c22 :: r23) 
    :: [] -> 
      let d1 = [c00 ; c11 ; c22] in 
      let d2 = [c02; c11 ; c20] in 
      let count = if (d1 = mas || d1 = sam)  && (d2 = mas || d2 = sam) then count + 1 else count in
      find_cross_horizontal_rec count
          [c01 :: c02 :: r03; 
           c11 :: c12 :: r13;
           c21 :: c22 :: r23]
    | _ -> count in
  find_cross_horizontal_rec 0

let part2 filename =   
  read_matrix filename |> window 3 |> Seq.map find_cross_horizontal |> sum_seq
 ;; 

part2 "inputs/d4.txt" |> print_int |> print_newline
