open Advent2024.Common

let parse lines =
  let parse_designs =
    Seq.flat_map @@ fun line ->
    String.split_on_char ',' line |> List.to_seq |> Seq.map String.trim
  in
  let designs =
    Seq.take_while (( <> ) "") lines
    |> parse_designs
    |> Seq.map (fun a -> (a, true))
    |> Hashtbl.of_seq
  in
  (designs, lines)

let combinations designs towel =
  let max_design =
    Hashtbl.to_seq_keys designs
    |> Seq.map String.length |> Seq.max_opt |> Option.get
  in
  let lengths = Seq.ints 1 |> Seq.take max_design in
  let memo = Fun.memo () in
  let rec aux towel =
    if towel = "" then 1
    else
      lengths
      |> Seq.filter (fun i -> String.length towel >= i)
      |> Seq.filter (fun i -> String.sub towel 0 i |> Hashtbl.mem designs)
      |> Seq.map (fun i -> String.length towel - i |> String.sub towel i)
      |> Seq.map (memo aux)
      |> Seq.sum
  in
  aux towel

let part1 =
  In_channel.with_open_text "inputs/d19.txt" @@ fun ic ->
  let design, towels = Seq.read_lines ic |> parse in
  towels
  |> Seq.map (combinations design)
  |> Seq.filter (fun c -> c > 0)
  |> Seq.length |> string_of_int |> print_endline

let part2 =
  In_channel.with_open_text "inputs/d19.txt" @@ fun ic ->
  let design, towels = Seq.read_lines ic |> parse in
  towels
  |> Seq.map (combinations design)
  |> Seq.sum |> string_of_int |> print_endline

let _ =
  part1;
  part2
