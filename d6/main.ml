module IntPairSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

module IntPairPairSet = Set.Make (struct
  type t = (int * int) * (int * int)

  let compare = compare
end)

let read_lines ic =
  let rec lines_rec ic () =
    match In_channel.input_line ic with
    | Some value -> Seq.Cons (value, lines_rec ic)
    | None -> Seq.Nil
  in
  lines_rec ic

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

  let rec tranverse_rec direction coord () =
    if not (in_bound coord) then Seq.Nil
    else
      let all_moves =
        directions direction |> Seq.map (fun d -> (d, move d coord))
      in
      let next_move =
        Seq.find
          (function
            | _, new_coord -> not (IntPairSet.mem new_coord obstructions))
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
  In_channel.with_open_text filename (fun ic -> read_lines ic |> parse)
  |> traverse |> IntPairSet.of_seq |> IntPairSet.to_seq |> Seq.length

let detect_loop nodes =
  let edges = Seq.zip nodes (Seq.drop 1 nodes) in
  let rec recurse set seq =
    match Seq.uncons seq with
    | Some (v, _) when IntPairPairSet.mem v set -> true
    | Some (v, next) -> recurse (IntPairPairSet.add v set) next
    | None -> false
  in
  recurse IntPairPairSet.empty edges

let part2 ?(parallel = false) filename =
  let dimension, starting, obstructions =
    In_channel.with_open_text filename (fun ic -> read_lines ic |> parse)
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
    |> Parmap.parmap (fun obstruction ->
           traverse_with obstruction |> detect_loop)
    |> List.filter (fun a -> a)
    |> List.length
  else
    IntPairSet.to_seq all_nodes
    |> Seq.filter (fun obstruction -> traverse_with obstruction |> detect_loop)
    |> Seq.length
;;

part1 "inputs/d6.txt" |> string_of_int |> print_endline;;
part2 ~parallel:true "inputs/d6.txt" |> string_of_int |> print_endline
