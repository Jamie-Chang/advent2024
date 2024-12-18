open Advent2024.Common
module IntPairSet = Set.Make (IntPair)

let parse_lines lines =
  Seq.map (String.split_on_char ',') lines
  |> Seq.map (List.map int_of_string)
  |> Seq.map (function [ x; y ] -> (y, x) | _ -> failwith "invalid input")

let bfs corrupted start target =
  let queue = Queue.create () in
  let _ = Queue.add [ start ] queue in
  let visisted = Hashtbl.create 10000 in

  let rec loop () =
    if Queue.is_empty queue then None
    else
      let path = Queue.pop queue in
      let location = List.hd path in
      if Hashtbl.mem visisted location then loop ()
      else
        let _ = Hashtbl.add visisted location true in
        if location = target then Some path
        else if IntPairSet.mem location corrupted then loop ()
        else
          let next =
            IntPair.directions
            |> Seq.map IntPair.(( + ) location)
            |> Seq.filter IntPair.(bound (target + (1, 1)))
          in
          let _ =
            next |> Seq.iter @@ fun next -> Queue.add (next :: path) queue
          in
          loop ()
  in
  loop ()

let corrupted =
  In_channel.with_open_text "inputs/d18.txt" @@ fun ic ->
  Seq.read_lines ic |> parse_lines |> List.of_seq

let find_path corrupted time =
  let corrupted = List.to_seq corrupted |> Seq.take time |> IntPairSet.of_seq in
  bfs corrupted IntPair.origin (70, 70)

let rec binary_search from until predicate =
  let index = (from + until) / 2 in
  match predicate index with
  | -1 -> binary_search from index predicate
  | 0 -> index
  | 1 -> binary_search (index + 1) until predicate
  | _ -> failwith "unexpected"

let predicate corrupted time =
  match (find_path corrupted time, find_path corrupted (time + 1)) with
  | None, None -> -1
  | Some _, None -> 0
  | Some _, Some _ -> 1
  | None, Some _ -> failwith "shouldn't happen"

let _ =
  find_path corrupted 1028 |> Option.get |> List.length |> Fun.flip ( - ) 1
  |> string_of_int |> print_endline;
  predicate corrupted
  |> binary_search 0 (List.length corrupted)
  |> List.nth corrupted
  |> fun (a, b) -> (b, a) |> IntPair.to_string |> print_endline
