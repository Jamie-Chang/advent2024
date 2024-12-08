open Advent2024
module CharMap = Map.Make (Char)

module IntPairSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

let uniq_length seq = IntPairSet.of_seq seq |> IntPairSet.to_seq |> Seq.length

let parse lines =
  let coords =
    Seq.mapi
      (fun r row -> String.to_seqi row |> Seq.map (fun (c, v) -> ((r, c), v)))
      lines
    |> Seq.concat
  in
  let fold_dimension _ (coord, _) = Common.IntPair.(coord + (1, 1)) in
  let fold_antennas acc (coord, v) =
    if v = '.' then acc
    else
      CharMap.update v
        (function
          | Some value -> Some (coord :: value) | None -> Some [ coord ])
        acc
  in
  Seq.fold_left
    (fun (dimension, antennas) value ->
      (fold_dimension dimension value, fold_antennas antennas value))
    (Common.IntPair.origin, CharMap.empty)
    coords

let calculate_antinodes (antenna1, antenna2) =
  let calculate_antinode c1 c2 = Common.IntPair.((c1 * 2) - c2) in
  [ calculate_antinode antenna1 antenna2; calculate_antinode antenna2 antenna1 ]
  |> List.to_seq

let rec combinations seq =
  match Seq.uncons seq with
  | Some (head, tail) ->
      let matches = tail |> Seq.map @@ fun v -> (head, v) in
      Seq.append matches @@ combinations tail
  | None -> Seq.empty

let ray start delta =
  let rec ray start delta () =
    Common.IntPair.(Seq.Cons (start, ray (start + delta) delta))
  in
  ray start delta

let part1 filename =
  In_channel.with_open_text filename @@ fun ic ->
  let bound, antennas = parse @@ Common.Seq.read_lines ic in
  let antinodes =
    CharMap.to_seq antennas
    |> Seq.flat_map (fun (_, locations) ->
           List.to_seq locations |> combinations
           |> Seq.flat_map calculate_antinodes)
  in
  antinodes
  |> Seq.filter (fun c -> Common.IntPair.(origin <= c && c < bound))
  |> uniq_length

let part2 filename =
  In_channel.with_open_text filename @@ fun ic ->
  let bound, antennas = parse @@ Common.Seq.read_lines ic in
  let in_bound v = Common.IntPair.(v >= origin && v < bound) in
  let ray start delta = ray start delta |> Seq.take_while in_bound in
  let antinodes (s, e) =
    Common.IntPair.(
      let delta = reduce (e - s) in
      Seq.append (ray s delta) (ray s (-delta)))
  in
  CharMap.to_seq antennas |> Seq.map snd
  |> Seq.flat_map (fun locations ->
         List.to_seq locations |> combinations |> Seq.flat_map antinodes)
  |> uniq_length

let filename = "inputs/d8.txt";;

assert (part1 filename = 390);;
assert (part2 filename = 1246)
