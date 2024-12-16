open Advent2024.Common
module IntPairMap = Map.Make (IntPair)

type tile = Box | Wall | Empty

let ( |>? ) res f = match res with None -> None | Some res -> Some (f res)

let parse_map lines =
  let coords =
    Seq.mapi
      (fun r row -> String.to_seqi row |> Seq.map (fun (c, v) -> ((r, c), v)))
      lines
    |> Seq.concat
  in
  let fold_starting acc (coord, v) =
    if acc <> None then acc else if v = '@' then Some coord else None
  in

  let fold_map acc (coord, v) =
    match v with
    | '#' -> IntPairMap.add coord Wall acc
    | '@' | '.' -> IntPairMap.add coord Empty acc
    | 'O' -> IntPairMap.add coord Box acc
    | _ -> failwith "invalid input"
  in

  let starting, map =
    Seq.fold_left
      (fun (starting, map) elem ->
        (fold_starting starting elem, fold_map map elem))
      (None, IntPairMap.empty) coords
  in

  (Option.get starting, map)

let parse_moves lines =
  let chars = Seq.flat_map @@ fun line -> String.to_seq line in
  let open IntPair in
  let translate = function
    | '<' -> left
    | '>' -> right
    | '^' -> up
    | 'v' -> down
    | _ -> failwith "invalid input"
  in
  chars lines |> Seq.map translate

let rec try_push map box direction =
  let next = IntPair.(box + direction) in
  match IntPairMap.find next map with
  | Empty -> Some next
  | Wall -> None
  | Box -> try_push map next direction

let try_move (location, map) move =
  let candidate = IntPair.(location + move) in
  match IntPairMap.find candidate map with
  | Wall -> (location, map)
  | Empty -> (candidate, map)
  | Box -> (
      match try_push map candidate move with
      | None -> (location, map)
      | Some next ->
          ( candidate,
            IntPairMap.add candidate Empty map |> IntPairMap.add next Box ))

let gps (r, c) = (100 * r) + c

let part1 filename =
  In_channel.with_open_text filename @@ fun ic ->
  let lines = Seq.read_lines ic in
  let initial = lines |> Seq.take_while (( <> ) "") |> parse_map in
  let moves = lines |> parse_moves in
  Seq.fold_left try_move initial moves
  |> snd |> IntPairMap.to_seq
  |> Seq.filter_map (function
       | location, Box -> Some (gps location)
       | _ -> None)
  |> Seq.sum

let _ = part1 "inputs/d15.txt" |> string_of_int |> print_endline

type widetile = LBox | Wall | Empty | RBox

let parse_map lines =
  let coords =
    Seq.mapi
      (fun r row -> String.to_seqi row |> Seq.map (fun (c, v) -> ((r, c), v)))
      lines
    |> Seq.concat
  in
  let fold_starting acc ((r, c), v) =
    let coord = (r, c * 2) in
    if acc <> None then acc else if v = '@' then Some coord else None
  in

  let fold_map acc (coord, v) =
    let first =
      let r, c = coord in
      (r, c * 2)
    in
    let second =
      let r, c = coord in
      (r, (c * 2) + 1)
    in
    let add tile1 tile2 =
      IntPairMap.add first tile1 acc |> IntPairMap.add second tile2
    in
    match v with
    | '#' -> add Wall Wall
    | '@' | '.' -> add Empty Empty
    | 'O' -> add LBox RBox
    | _ -> failwith "invalid input"
  in

  let starting, map =
    Seq.fold_left
      (fun (starting, map) elem ->
        (fold_starting starting elem, fold_map map elem))
      (None, IntPairMap.empty) coords
  in

  (Option.get starting, map)

let parse_moves lines =
  let chars = Seq.flat_map @@ fun line -> String.to_seq line in
  let open IntPair in
  let translate = function
    | '<' -> left
    | '>' -> right
    | '^' -> up
    | 'v' -> down
    | _ -> failwith "invalid input"
  in
  chars lines |> Seq.map translate

let move_point direction start map =
  let value = IntPairMap.find start map in
  let removed = map |> IntPairMap.update start @@ fun _ -> Some Empty in
  removed |> IntPairMap.add IntPair.(start + direction) value

let move_box direction (l, r) map =
  map |> move_point direction l |> move_point direction r

let rec try_push_verticle direction map (l, r) =
  match map with
  | None -> None
  | Some map -> (
      let open IntPair in
      let new_left = l + direction in
      let new_right = r + direction in
      let next_boxes =
        match (IntPairMap.find new_left map, IntPairMap.find new_right map) with
        | LBox, RBox -> Some [ (new_left, new_right) ]
        | RBox, Empty -> Some [ (new_left + left, new_left) ]
        | RBox, LBox ->
            Some [ (new_left + left, new_left); (new_right, new_right + right) ]
        | Empty, LBox -> Some [ (new_right, new_right + right) ]
        | Empty, Empty -> Some []
        | Wall, _ | _, Wall -> None
        | LBox, (LBox | Empty) | (RBox | Empty), RBox ->
            failwith "Invalid box combination"
      in
      match next_boxes with
      | Some next_boxes ->
          List.fold_left
            (fun next_map next_box ->
              try_push_verticle direction next_map next_box)
            (Some map) next_boxes
          |>? move_box direction (l, r)
      | None -> None)

let rec try_move_horizontal direction map location =
  let candidate = IntPair.(location + direction) in
  match IntPairMap.find candidate map with
  | Empty -> Some (move_point direction location map)
  | Wall -> None
  | LBox | RBox ->
      try_move_horizontal direction map candidate
      |>? move_point direction location

let try_move (location, map) direction =
  let candidate = IntPair.(location + direction) in
  match IntPairMap.find candidate map with
  | Wall -> (location, map)
  | Empty -> (candidate, map)
  | LBox when direction = IntPair.up || direction = IntPair.down -> (
      match
        try_push_verticle direction (Some map)
          (candidate, IntPair.(candidate + right))
      with
      | Some map -> (candidate, map)
      | None -> (location, map))
  | RBox when direction = IntPair.up || direction = IntPair.down -> (
      match
        try_push_verticle direction (Some map)
          (IntPair.(candidate + left), candidate)
      with
      | Some map -> (candidate, map)
      | None -> (location, map))
  | LBox | RBox -> (
      match try_move_horizontal direction map location with
      | Some map -> (candidate, map)
      | None -> (location, map))

let part2 filename =
  In_channel.with_open_text filename @@ fun ic ->
  let lines = Seq.read_lines ic in
  let initial = lines |> Seq.take_while (( <> ) "") |> parse_map in
  let moves = lines |> parse_moves in
  Seq.fold_left try_move initial moves
  |> snd |> IntPairMap.to_seq
  |> Seq.filter_map (function
       | location, LBox -> Some (gps location)
       | _ -> None)
  |> Seq.sum |> string_of_int |> print_endline

let _ = part2 "inputs/d15.txt"
