open Advent2024.Common

type schematic = Lock of int Array.t | Key of int Array.t

let parse_schematic grid =
  let range x = Seq.ints 0 |> Seq.take x in
  if Array.get grid 0 |> Array.for_all (( = ) '.') then
    let columns =
      range 5
      |> Seq.map (fun c ->
             range 7
             |> Seq.map (fun x -> 6 - x)
             |> Seq.map (fun r -> Array.get grid r |> Fun.flip Array.get c))
    in
    columns
    |> Seq.map (Seq.take_while (fun x -> x = '#'))
    |> Seq.map Seq.length |> Array.of_seq
    |> fun x -> Key x
  else
    let columns =
      range 5
      |> Seq.map (fun c ->
             range 7
             |> Seq.map (fun r -> Array.get grid r |> Fun.flip Array.get c))
    in
    columns
    |> Seq.map (Seq.take_while (fun x -> x = '#'))
    |> Seq.map Seq.length |> Array.of_seq
    |> fun x -> Lock x

let parse lines =
  Seq.map String.to_seq lines |> Seq.map Array.of_seq |> Array.of_seq
  |> fun a -> if Array.length a = 0 then None else Some (parse_schematic a)

let fit lock key =
  Seq.zip (Array.to_seq lock) (Array.to_seq key)
  |> Seq.for_all (fun (x, y) -> x + y <= 7)

let group schematics =
  Seq.fold_left
    (fun (locks, keys) v ->
      match v with Lock v -> (v :: locks, keys) | Key v -> (locks, v :: keys))
    ([], []) schematics

let part1 filename =
  In_channel.with_open_text filename @@ fun ic ->
  let lines = Seq.read_lines ic in
  let rec parse_each lines () =
    match Seq.take_while (( <> ) "") lines |> parse with
    | Some v -> Seq.Cons (v, parse_each lines)
    | None -> Seq.empty ()
  in
  parse_each lines |> group |> fun (locks, keys) ->
  Seq.product (List.to_seq locks) (List.to_seq keys)
  |> Seq.filter (fun (lock, key) -> fit lock key)
  |> Seq.length |> string_of_int |> print_endline

let _ = part1 "inputs/d25.txt"
