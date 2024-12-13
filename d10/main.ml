open Advent2024


module IntPairMap = Map.Make (Common.IntPair)
module IntPairSet = Set.Make (Common.IntPair)

let parse_topograph lines =
  let parse_char c = int_of_char c |> fun i -> i - 48 in
  let coords =
    Seq.mapi
      (fun r row ->
        String.to_seqi row |> Seq.map (fun (c, v) -> ((r, c), parse_char v)))
      lines
    |> Seq.concat
  in
  IntPairMap.of_seq coords

let find_origins topograph =
  IntPairMap.to_seq topograph
  |> Seq.filter_map @@ fun (k, v) -> if v = 0 then Some k else None

let find_bound topograph =
  IntPairMap.to_rev_seq topograph |> Seq.uncons |> function
  | Some ((coord, _), _) -> Common.IntPair.(coord + (1, 1))
  | None -> failwith "empty topograph"

let moves coord =
  let open Common.IntPair in
  directions |> Seq.map @@ ( + ) coord

let visit graph (bx, by) origin =
  let inbound (x, y) = x >= 0 && x < bx && y >= 0 && y < by in
  let pathable current_height next_coord =
    IntPairMap.find next_coord graph = current_height + 1
  in
  let rec visit_aux coord =
    let current_height = IntPairMap.find coord graph in
    if current_height = 9 then List.to_seq [ coord ]
    else
      let valid_moves =
        moves coord |> Seq.filter inbound
        |> Seq.filter @@ pathable current_height
      in
      valid_moves |> Seq.flat_map @@ visit_aux
  in
  visit_aux origin

let part1 filename =
  let topograph =
    In_channel.with_open_text filename
    @@ Fun.compose parse_topograph Common.Seq.read_lines
  in
  let bound = find_bound topograph in
  let origins = find_origins topograph in
  let unique seq = seq |> IntPairSet.of_seq |> IntPairSet.to_seq in
  origins
  |> Seq.map @@ visit topograph bound
  |> Seq.map unique |> Seq.map Seq.length |> Common.Seq.sum |> string_of_int

let part2 filename =
  let topograph =
    In_channel.with_open_text filename
    @@ Fun.compose parse_topograph Common.Seq.read_lines
  in
  let bound = find_bound topograph in
  let origins = find_origins topograph in
  origins
  |> Seq.map @@ visit topograph bound
  |> Seq.map Seq.length |> Common.Seq.sum |> string_of_int
;;

part1 "inputs/d10.txt" |> print_endline;;
part2 "inputs/d10.txt" |> print_endline
