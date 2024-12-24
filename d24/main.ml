open Advent2024.Common
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type wire = Literal of int | Gate of string * (int -> int -> int) * string

let parse_lines lines =
  let parse_gate = function
    | "AND" -> Int.logand
    | "XOR" -> Int.logxor
    | "OR" -> Int.logor
    | _ -> failwith "invalid gate"
  in

  let initial_values =
    Seq.take_while (( <> ) "") lines
    |> Seq.map @@ fun line ->
       String.split_on_char ':' line |> List.map String.trim |> function
       | [ w; v ] -> (w, Literal (int_of_string v))
       | _ -> failwith "invalid input"
  in

  let connections =
    lines
    |> Seq.map @@ fun line ->
       String.split_on_char ' ' line |> function
       | [ w1; op; w2; _; wire ] -> (wire, Gate (w1, parse_gate op, w2))
       | _ -> failwith "invalid input"
  in
  Seq.append initial_values connections |> StringMap.of_seq

let rec compute wire_map =
  let calculate key value (acc, changed) =
    match value with
    | Literal s -> (StringMap.add key (Literal s) acc, changed)
    | Gate (w1, g, w2) -> (
        match (StringMap.find w1 wire_map, StringMap.find w2 wire_map) with
        | Literal v1, Literal v2 ->
            (StringMap.add key (Literal (g v1 v2)) acc, true)
        | _ -> (StringMap.add key (Gate (w1, g, w2)) acc, changed))
  in
  let new_map, changed =
    StringMap.fold calculate wire_map (StringMap.empty, false)
  in
  if changed then compute new_map else new_map

let read_output wire_map =
  StringMap.to_rev_seq wire_map
  |> Seq.filter (fun (k, _) -> String.starts_with ~prefix:"z" k)
  |> Seq.map snd
  |> Seq.fold_left (fun acc v -> Int.(shift_left acc 1 lor v)) 0

let part1 filename =
  In_channel.with_open_text filename @@ fun ic ->
  Seq.read_lines ic |> parse_lines |> compute
  |> StringMap.map (function
       | Gate _ -> failwith "not computed"
       | Literal v -> v)
  |> read_output |> string_of_int |> print_endline

let _ = part1 "inputs/d24.txt"

let zeroed =
  let numbers = Seq.ints 0 |> Seq.take 45 in
  let x_keys = Seq.map (Printf.sprintf "x%02d") numbers in
  let y_keys = Seq.map (Printf.sprintf "y%02d") numbers in
  let xs = Seq.map (fun k -> (k, Literal 0)) x_keys in
  let ys = Seq.map (fun k -> (k, Literal 0)) y_keys in
  Seq.append xs ys

let formatted n = Printf.sprintf "%02d" n
let x_key n = "x" ^ formatted n
let y_key n = "y" ^ formatted n
let z_key n = "z" ^ formatted n

let get_candidates wires n =
  let wires = StringMap.add_seq zeroed wires in
  [ (0, 1); (1, 1) ]
  |> List.to_seq
  |> Seq.map (fun (x, y) -> [ (x_key n, Literal x); (y_key n, Literal y) ])
  |> Seq.map List.to_seq
  |> Seq.map (Fun.flip StringMap.add_seq wires)
  |> Seq.map compute
  |> Seq.flat_map StringMap.to_seq
  |> Seq.filter (fun (_, v) -> v = Literal 1)
  |> Seq.map fst
  |> Seq.append (Seq.return @@ z_key n)

let check wires n =
  if n >= 0 && n < 45 then
    let wires = StringMap.add_seq zeroed wires in
    let test_values = [ ((0, 1), 1); ((1, 1), 0) ] in
    let check ((x, y), z) =
      let wires =
        [ (x_key n, Literal x); (y_key n, Literal y) ]
        |> List.to_seq
        |> Fun.flip StringMap.add_seq wires
        |> compute
      in
      StringMap.find (z_key n) wires = Literal z
    in
    List.for_all check test_values
  else true

let check_next wires n =
  if n >= 0 then
    let wires = StringMap.add_seq zeroed wires in
    let test_values =
      [
        (((0, 1), 1), ((0, 1), 1));
        (((0, 1), 1), ((1, 1), 0));
        (((1, 1), 0), ((0, 1), 0));
        (((1, 1), 0), ((1, 1), 1));
      ]
    in
    let check (curr, next) =
      let update_wires ((x, y), _) n wires =
        [ (x_key n, Literal x); (y_key n, Literal y) ]
        |> List.to_seq
        |> Fun.flip StringMap.add_seq wires
      in
      let wires = update_wires curr n wires |> update_wires next (n + 1) in
      let computed = compute wires in
      StringMap.find (z_key n) computed = Literal (snd curr)
      && StringMap.find (z_key (n + 1)) computed = Literal (snd next)
    in
    List.for_all check test_values
  else true

let fix wires candidates n =
  let rec combinations seq =
    match Seq.uncons seq with
    | Some (head, tail) ->
        Seq.(map (fun x -> (head, x)) tail |> append (combinations tail))
    | None -> Seq.empty
  in
  let swap map (x, y) =
    let vx = StringMap.find x map in
    let vy = StringMap.find y map in
    StringMap.add x vy map |> StringMap.add y vx
  in
  StringSet.to_seq candidates
  |> combinations
  |> Seq.map (fun x -> (swap wires x, x))
  |> Seq.filter (fun (wires, _) ->
         check wires n && check_next wires n && check_next wires (n - 1))
  |> Seq.uncons
  |> function
  | Some (hd, rst) when Seq.is_empty rst -> Some hd
  | Some _ -> failwith "more than one swap available"
  | None -> failwith "more than one swap needed"

let part2 filename =
  let wires =
    In_channel.with_open_text filename @@ fun ic ->
    Seq.read_lines ic |> parse_lines
  in
  let numbers = Seq.ints 1 |> Seq.take 44 in
  Seq.fold_left
    (fun (wires, swaps) num ->
      if not (check wires num) then
        let candidates = get_candidates wires num |> StringSet.of_seq in
        match fix wires candidates num with
        | Some (wires, swap) -> (wires, swap :: swaps)
        | None -> (wires, swaps)
      else (wires, swaps))
    (wires, []) numbers
  |> snd |> List.to_seq
  |> Seq.flat_map (fun (x, y) -> Seq.(append (return x) (return y)))
  |> List.of_seq |> List.sort_uniq compare |> String.concat ","

let _ = part2 "inputs/d24.txt" |> print_endline
