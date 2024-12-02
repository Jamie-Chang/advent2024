open Iter
module IntMap = Map.Make (Int)

let parse_line x =
  String.split_on_char ' ' x
  |> List.filter (fun x -> x <> "")
  |> List.map int_of_string

let rec check_levels direction levels =
  match levels with
  | [] -> true
  | _ :: [] -> true
  | first :: second :: others ->
      let delta = (second - first) * direction in
      if delta > 0 && delta <= 3 then check_levels direction (second :: others)
      else false

let part1 filename =
  let reports = IO.lines_of filename |> map (fun x -> parse_line x) in
  map (fun x -> check_levels 1 x || check_levels (-1) x) reports
  |> filter_count (fun x -> x)
;;

part1 "inputs/d2.txt" |> string_of_int |> print_endline

let rec check_levels_with_dampener direction levels =
  match levels with
  | [] -> true
  | _ :: [] -> true
  | first :: second :: others ->
      let delta = (second - first) * direction in
      if delta > 0 && delta <= 3 then
        check_levels_with_dampener direction (second :: others)
      else check_levels direction (first :: others)

let part2 filename =
  let reports = IO.lines_of filename |> map (fun x -> parse_line x) in
  map
    (fun x ->
      check_levels_with_dampener 1 x
      || check_levels_with_dampener (-1) x
      ||
      let reversed = List.rev x in
      check_levels_with_dampener (-1) reversed
      || check_levels_with_dampener 1 reversed)
    reports
  |> filter_count (fun x -> x)
;;

part2 "inputs/d2.txt" |> string_of_int |> print_endline
