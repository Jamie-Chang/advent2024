open Advent2024.Common
module IntPairSet = Set.Make (IntPair)

module Garden = struct
  module IntPairMap = Map.Make (IntPair)
  include IntPairMap

  let of_lines lines =
    lines
    |> Seq.mapi (fun r row ->
           String.to_seq row |> Seq.mapi @@ fun c cell -> ((r, c), cell))
    |> Seq.concat |> IntPairMap.of_seq

  let to_key_seq garden = to_seq garden |> Seq.map fst

  let get_bound garden =
    let max =
      to_key_seq garden
      |> Seq.fold_left
           (fun prev next ->
             if IntPair.(compare prev next) = -1 then next else prev)
           (0, 0)
    in
    IntPair.(max + (1, 1))
end

let neighbours coord = IntPair.directions |> Seq.map IntPair.(( + ) coord)

let get_region garden start =
  let plant_type = Garden.find start garden in
  let in_bound = IntPair.bound @@ Garden.get_bound garden in
  let rec aux (visited, nodes) node =
    if IntPairSet.mem node visited then (visited, nodes)
    else if Garden.find node garden <> plant_type then
      (IntPairSet.add node visited, nodes)
    else
      let nodes = Seq.cons node nodes in
      neighbours node |> Seq.filter in_bound
      |> Seq.fold_left aux (IntPairSet.add node visited, nodes)
  in
  aux (IntPairSet.empty, Seq.empty) start |> snd

let node_perimeter garden plant_type node =
  let perimeter_nodes =
    neighbours node
    |> Seq.filter @@ fun n ->
       match Garden.find_opt n garden with
       | Some p when p <> plant_type -> true
       | None -> true
       | _ -> false
  in
  Seq.length perimeter_nodes

let to_regions garden =
  let nodes = Garden.to_key_seq garden |> IntPairSet.of_seq in
  let rec consume nodes =
    match IntPairSet.choose_opt nodes with
    | Some v ->
        let region = get_region garden v |> IntPairSet.of_seq in
        Seq.cons region @@ consume @@ IntPairSet.diff nodes region
    | None -> Seq.empty
  in

  consume nodes

let part1 garden =
  let price region =
    let plant_type = IntPairSet.choose region |> Fun.flip Garden.find garden in
    let perimeter =
      IntPairSet.fold
        (fun node p -> p + node_perimeter garden plant_type node)
        region 0
    in
    let area = IntPairSet.to_seq region |> Seq.length in
    area * perimeter
  in
  to_regions garden |> Seq.map price |> Seq.sum

let side direction nodes =
  IntPairSet.to_seq nodes
  |> Seq.filter @@ fun n -> IntPairSet.mem IntPair.(n + direction) nodes |> not

let rec take direction side sides =
  if IntPairSet.mem side sides then
    take direction IntPair.(side + direction) @@ IntPairSet.remove side sides
  else sides

let sides_of region =
  let rotate (x, y) = (-y, x) in
  let count_sides direction sides =
    let rec aux count sides =
      match IntPairSet.choose_opt sides with
      | None -> (count, sides)
      | Some s ->
          aux (count + 1)
            IntPair.(
              take direction s sides |> take (-direction) (s - direction))
    in
    aux 0 sides |> fst
  in
  IntPair.(
    directions
    |> Seq.map @@ fun d ->
       side d region |> IntPairSet.of_seq |> count_sides (rotate d))
  |> Seq.sum

let part2 garden =
  let price region =
    let sides = sides_of region in
    let area = IntPairSet.to_seq region |> Seq.length in
    area * sides
  in
  to_regions garden |> Seq.map price |> Seq.sum

let _ =
  let file = "inputs/d12.txt" in
  let garden =
    In_channel.with_open_text file @@ fun ic ->
    Seq.read_lines ic |> Garden.of_lines
  in
  (part1 garden, part2 garden) |> IntPair.to_string |> print_endline
