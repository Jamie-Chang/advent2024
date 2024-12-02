open Iter
module IntMap = Map.Make (Int)

let parse_line x =
  String.split_on_char ' ' x
  |> List.filter (fun x -> x <> "")
  |> List.map int_of_string

let accumulate_pair (acc1, acc2) values =
  match values with
  | [ x; y ] -> (x :: acc1, y :: acc2)
  | _ -> failwith "Invalid input"

let part1 filename =
  let pairs = IO.lines_of filename |> map (fun x -> parse_line x) in
  let first, second = fold accumulate_pair ([], []) pairs in
  let first_sorted, second_sorted =
    (List.sort compare first, List.sort compare second)
  in
  let accumulate acc a b = acc + abs (a - b) in
  List.fold_left2 accumulate 0 first_sorted second_sorted
;;

part1 "inputs/d1.txt" |> string_of_int |> print_endline

let create_counter list =
  let increment old =
    match old with None -> Some 1 | Some x -> Some (x + 1)
  in

  List.fold_left (fun acc x -> IntMap.update x increment acc) IntMap.empty list

let part2 filename =
  let pairs = IO.lines_of filename |> map (fun x -> parse_line x) in
  let first, second = fold accumulate_pair ([], []) pairs in
  let first, second = (List.sort compare first, List.sort compare second) in
  let counter = create_counter second in
  List.fold_left
    (fun acc x ->
      match IntMap.find_opt x counter with
      | Some y -> acc + (x * y)
      | None -> acc)
    0 first
;;

part2 "inputs/d1.txt" |> string_of_int |> print_endline
