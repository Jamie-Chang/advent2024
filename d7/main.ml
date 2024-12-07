open Advent2024

let parse_line line =
  match String.split_on_char ' ' line with
  | x :: xs ->
      ( String.split_on_char ':' x |> Fun.flip List.nth 0 |> int_of_string,
        List.map int_of_string xs )
  | _ -> failwith "failed to parse"
;;

let check operators test =
  let rec recursion operators test total value =
    match value with
    | x :: xs ->
        List.to_seq operators
        |> Seq.map (fun op -> op total x) (* new total *)
        |> Seq.filter (fun new_total -> new_total <= test)
        |> Seq.map (fun total -> recursion operators test total xs)
        |> Seq.exists Fun.id
    | [] -> total = test
  in
  function x :: xs -> recursion operators test x xs | _ -> true

let part1 filename =
  let operators = [ ( + ); ( * ) ] in
  In_channel.with_open_text filename (fun ic ->
      Common.Seq.read_lines ic |> Seq.map parse_line
      |> Seq.filter_map (fun (test_val, numbers) ->
             if check operators test_val numbers then Some test_val else None)
      |> Common.Seq.sum)
;;

part1 "inputs/d7.txt" |> string_of_int |> print_endline

let part2 filename =
  let ( || ) a b = string_of_int a ^ string_of_int b |> int_of_string in
  let operators = [ ( + ); ( * ); ( || ) ] in
  In_channel.with_open_text filename (fun ic ->
      Common.Seq.read_lines ic |> Seq.map parse_line
      |> Seq.filter_map (fun (test_val, numbers) ->
             if check operators test_val numbers then Some test_val else None)
      |> Common.Seq.sum)
;;

part2 "inputs/d7.txt" |> string_of_int |> print_endline
