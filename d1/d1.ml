open Advent2024
module IntMap = Map.Make (Int)

let parse_line x =
  String.split_on_char ' ' x
  |> List.filter @@ ( <> ) ""
  |> List.map int_of_string
  |> function
  | [ a; b ] -> (a, b)
  | _ -> failwith "malformed input"

let part1 filename =
  In_channel.with_open_text filename @@ fun ic ->
  let first, second =
    Common.Seq.read_lines ic |> Seq.map parse_line
    |> Fun.compose Seq.unzip Seq.memoize
  in
  let first, second =
    ( List.sort compare @@ List.of_seq first,
      List.sort compare @@ List.of_seq second )
  in
  List.fold_left2 (fun acc a b -> acc + abs (a - b)) 0 first second
;;

part1 "inputs/d1.txt" |> string_of_int |> print_endline

let create_counter list =
  let increment = function None -> Some 1 | Some x -> Some (x + 1) in
  List.fold_left (fun acc x -> IntMap.update x increment acc) IntMap.empty list

let part2 filename =
  In_channel.with_open_text filename @@ fun ic ->
  let first, second =
    Common.Seq.read_lines ic |> Seq.map parse_line
    |> Fun.compose Seq.unzip Seq.memoize
  in
  let second = List.sort compare @@ List.of_seq second in
  let counter = create_counter second in
  Seq.map
    (fun x ->
      match IntMap.find_opt x counter with Some y -> x * y | None -> 0)
    first
  |> Common.Seq.sum
;;

part2 "inputs/d1.txt" |> string_of_int |> print_endline
