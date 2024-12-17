open Advent2024.Common
module IntPairSet = Set.Make (IntPair)

module IntPairPairMap = Map.Make (struct
  type t = (int * int) * (int * int)

  let compare = compare
end)

let parse_map lines =
  let coords =
    Seq.mapi
      (fun r row -> String.to_seqi row |> Seq.map (fun (c, v) -> ((r, c), v)))
      lines
    |> Seq.concat
  in
  let fold_start acc (coord, v) =
    if acc <> None then acc else if v = 'S' then Some coord else None
  in

  let fold_end acc (coord, v) =
    if acc <> None then acc else if v = 'E' then Some coord else None
  in

  let fold_map acc (coord, v) =
    match v with
    | '.' | 'S' | 'E' -> IntPairSet.add coord acc
    | '#' -> acc
    | _ -> failwith "invalid input"
  in

  let starting, ending, map =
    Seq.fold_left
      (fun (starting, ending, map) elem ->
        (fold_start starting elem, fold_end ending elem, fold_map map elem))
      (None, None, IntPairSet.empty)
      coords
  in

  (Option.get starting, Option.get ending, map)

let walk start finish maze =
  let rec reachable ((node, direction), (cost, nodes)) =
    let open IntPair in
    if node = finish then Seq.return ((node, direction), (cost, nodes))
    else
      let left, right = orthogonal direction in
      let turns =
        [ left; right ] |> List.to_seq
        |> Seq.filter_map (fun dir ->
               if IntPairSet.mem (node + dir) maze then Some (node + dir, dir)
               else None)
        |> Seq.map @@ fun (n, d) ->
           ((n, d), Stdlib.(cost + 1001, IntPairSet.add n nodes))
      in
      let next = node + direction in
      let next_step =
        if IntPairSet.mem next maze then
          reachable
            ((next, direction), Stdlib.(cost + 1, IntPairSet.add next nodes))
        else Seq.empty
      in
      Seq.append turns next_step
  in

  let search already_reached reached =
    IntPairPairMap.to_seq reached
    |> Seq.flat_map reachable
    |> Seq.filter (fun (key, _) ->
           IntPairPairMap.mem key already_reached |> not)
    |> Seq.fold_left
         (fun map (vertex, (cost, nodes)) ->
           IntPairPairMap.update vertex
             (function
               | Some (c, n) ->
                   if cost < c then Some (cost, nodes)
                   else if cost = c then Some (cost, IntPairSet.union nodes n)
                   else Some (c, n)
               | None -> Some (cost, nodes))
             map)
         IntPairPairMap.empty
  in
  let rec iter_step already_reached reached =
    let next_reached = search already_reached reached in
    let finish_cost =
      IntPair.directions
      |> Seq.filter_map (fun d ->
             IntPairPairMap.find_opt (finish, d) next_reached)
      |> Seq.min_opt
    in
    match finish_cost with
    | Some v -> v
    | None ->
        iter_step
          ( IntPairPairMap.to_seq next_reached |> fun s ->
            IntPairPairMap.add_seq s already_reached )
          next_reached
  in
  let cost, nodes =
    IntPairPairMap.(
      iter_step empty
        (empty
        |> add (start, IntPair.right) (0, IntPairSet.(empty |> add start))))
  in

  (cost, IntPairSet.to_seq nodes |> Seq.length)

let _ =
  let start, finish, map =
    In_channel.with_open_text "inputs/d16.txt" @@ fun ic ->
    Seq.read_lines ic |> parse_map
  in
  walk start finish map |> IntPair.to_string |> print_endline
