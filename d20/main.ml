open Advent2024.Common
module IntPairSet = Set.Make (IntPair)
module IntPairMap = Map.Make (IntPair)

let parse lines =
  let points =
    lines
    |> Seq.mapi (fun i line ->
           String.to_seq line |> Seq.mapi (fun j c -> ((i, j), c)))
    |> Seq.concat |> Seq.memoize
  in
  let find value =
    Seq.find_map (fun (p, v) -> if v = value then Some p else None) points
    |> Option.get
  in
  ( points
    |> Seq.filter_map (fun (p, c) -> if c <> '#' then Some p else None)
    |> IntPairSet.of_seq,
    find 'S',
    find 'E' )


let cheat_vectors limit = 
  let cheat_vectors limit =
    let limit = limit in
    let ( --= ) s f = Seq.ints s |> Seq.take_while (fun n -> n <= f) in
    let points =
      1 --= limit
      |> Seq.flat_map @@ fun r ->
         0 --= (limit - r)
         |> Seq.map (fun c -> (r, c))
         |> Seq.filter (( <> ) (1, 0))
    in
    let rotate (x, y) = (-y, x) in
    let ( $ ) = Fun.compose in
    let ( @ ) = Seq.append in
    points @ Seq.map rotate points
    @ Seq.map (rotate $ rotate) points
    @ Seq.map (rotate $ rotate $ rotate) points
  in  
  let distance (r, c) = abs r + abs c in
  cheat_vectors limit |> Seq.map @@ fun p -> (p, distance p) 

let savings limit (grid, start, finish) =
  let rec race last current () =
    if current = finish then Seq.Cons (finish, Seq.empty)
    else
      let next =
        IntPair.(
          directions
          |> Seq.map (( + ) current)
          |> Seq.filter (fun n -> Some n <> last)
          |> Seq.filter (Fun.flip IntPairSet.mem grid))
        |> Seq.uncons |> Option.get |> fst
      in
      Seq.Cons (current, race (Some current) next)
  in
  let distances =
    race None start |> Seq.mapi (fun i p -> (p, i)) |> IntPairMap.of_seq
  in
  let cheat_vectors = cheat_vectors limit |> Seq.memoize in
  IntPairMap.to_seq distances
  |> Seq.filter (fun (p, _) -> p <> finish)
  |> Seq.flat_map @@ fun (p, d) ->
     cheat_vectors
     |> Seq.map (fun (node, cost) -> (IntPair.(node + p), cost))
     |> Seq.filter (fun (node, _) -> IntPairMap.mem node distances)
     |> Seq.map (fun (node, cost) -> IntPairMap.find node distances - d - cost)

let main filename =
  let parsed =
    In_channel.with_open_text filename @@ fun ic -> Seq.read_lines ic |> parse
  in
  ( parsed |> savings 2 |> Seq.filter (fun n -> n >= 100) |> Seq.length,
    parsed |> savings 20 |> Seq.filter (fun n -> n >= 100) |> Seq.length )

let _ = main "inputs/d20.txt" |> IntPair.to_string |> print_endline
