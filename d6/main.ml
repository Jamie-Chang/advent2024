open Advent2024

module IntPairSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

module IntPairPairSet = Set.Make (struct
  type t = (int * int) * (int * int)

  let compare = compare
end)

let parse lines =
  let coords =
    Seq.mapi
      (fun r row -> String.to_seqi row |> Seq.map (fun (c, v) -> ((r, c), v)))
      lines
    |> Seq.concat
  in
  let fold_dimension _ ((x, y), _) = (x + 1, y + 1) in
  let fold_starting acc (coord, v) = if v = '^' then coord else acc in
  let fold_obstructions obstructions (coord, v) =
    if v = '#' then IntPairSet.add coord obstructions else obstructions
  in
  Seq.fold_left
    (fun (dimension, starting, obstructions) elem ->
      ( fold_dimension dimension elem,
        fold_starting starting elem,
        fold_obstructions obstructions elem ))
    ((0, 0), (0, 0), IntPairSet.empty)
    coords

let move direction coord =
  let dx, dy = direction in
  let x, y = coord in
  (x + dx, y + dy)

let up = (-1, 0)
let right = (0, 1)
let down = (1, 0)
let left = (0, -1)

let turn direction =
  match direction with
  | d when d = up -> right
  | d when d = right -> down
  | d when d = down -> left
  | d when d = left -> up
  | _ -> failwith "unexpected direction"

let directions starting =
  [
    starting;
    starting |> turn;
    starting |> turn |> turn;
    starting |> turn |> turn |> turn;
  ]
  |> List.to_seq

let traverse (dimension, starting, obstructions) =
  let x_bound, y_bound = dimension in

  let in_bound = function
    | x, y -> x < x_bound && y < y_bound && x >= 0 && y >= 0
  in

  let obstructed coord = IntPairSet.mem coord obstructions in

  let rec tranverse_rec direction coord () =
    if not @@ in_bound coord then Seq.Nil
    else
      let all_moves =
        directions direction |> Seq.map (fun d -> (d, move d coord))
      in
      let next_move =
        Seq.find
          (function _, new_coord -> not @@ obstructed new_coord)
          all_moves
      in

      match next_move with
      | Some (direction, next_coord) ->
          Seq.Cons (coord, tranverse_rec direction next_coord)
      | _ -> failwith "boxed in"
  in
  tranverse_rec up starting

let _format_tuple (x, y) = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let part1 filename =
  In_channel.with_open_text filename @@ Fun.compose parse Common.Seq.read_lines
  |> traverse |> IntPairSet.of_seq |> IntPairSet.to_seq |> Seq.length

let detect_loop nodes =
  let edges = Seq.zip nodes @@ Seq.drop 1 nodes in
  let rec recurse set seq =
    match Seq.uncons seq with
    | Some (v, _) when IntPairPairSet.mem v set -> true
    | Some (v, next) -> recurse (IntPairPairSet.add v set) next
    | None -> false
  in
  recurse IntPairPairSet.empty edges

let part2 ?(parallel = false) filename =
  let dimension, starting, obstructions =
    In_channel.with_open_text filename @@ Fun.compose parse Common.Seq.read_lines
  in
  let all_nodes =
    traverse (dimension, starting, obstructions)
    |> IntPairSet.of_seq |> IntPairSet.remove starting
  in
  let traverse_with obstruction =
    traverse (dimension, starting, IntPairSet.add obstruction obstructions)
  in
  if parallel then
    Parmap.L (IntPairSet.to_list all_nodes)
    |> Parmap.parmap @@ Fun.compose detect_loop traverse_with
    |> List.filter Fun.id |> List.length
  else
    IntPairSet.to_seq all_nodes
    |> Seq.filter (Fun.compose detect_loop traverse_with)
    |> Seq.length
;;

part1 "inputs/d6.txt" |> string_of_int |> print_endline;;
part2 ~parallel:true "inputs/d6.txt" |> string_of_int |> print_endline
