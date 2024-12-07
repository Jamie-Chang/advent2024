open Advent2024

let parse_line x =
  String.split_on_char ' ' x
  |> List.filter @@ ( <> ) ""
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
  In_channel.with_open_text filename @@ fun ic ->
  Common.Seq.read_lines ic |> Seq.map parse_line
  |> Seq.map (fun x -> check_levels 1 x || check_levels (-1) x)
  |> Seq.filter Fun.id |> Seq.length
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
  In_channel.with_open_text filename @@ fun ic ->
  Common.Seq.read_lines ic |> Seq.map parse_line
  |> Seq.map (fun x ->
         let reversed = List.rev x in
         check_levels_with_dampener 1 x
         || check_levels_with_dampener (-1) x
         || check_levels_with_dampener (-1) reversed
         || check_levels_with_dampener 1 reversed)
  |> Seq.filter Fun.id |> Seq.length
;;

part2 "inputs/d2.txt" |> string_of_int |> print_endline
