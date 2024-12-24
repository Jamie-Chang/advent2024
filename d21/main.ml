open Advent2024.Common

let npad_moves (prev, target) =
  let move_to_dpad (r, c) =
    let up = Seq.repeat '^' |> Seq.take (max (-r) 0) in
    let down = Seq.repeat 'v' |> Seq.take (max r 0) in
    let left = Seq.repeat '<' |> Seq.take (max (-c) 0) in
    let right = Seq.repeat '>' |> Seq.take (max c 0) in
    let ( @ ) = Seq.append in
    left @ down @ up @ right
  in
  let numpad c =
    match c with
    | '7' -> (0, 0)
    | '8' -> (0, 1)
    | '9' -> (0, 2)
    | '4' -> (1, 0)
    | '5' -> (1, 1)
    | '6' -> (1, 2)
    | '1' -> (2, 0)
    | '2' -> (2, 1)
    | '3' -> (2, 2)
    | '0' -> (3, 1)
    | 'A' -> (3, 2)
    | _ -> failwith "invalid code"
  in
  let move prev target =
    IntPair.(numpad target - numpad prev) |> move_to_dpad |> String.of_seq
  in
  let moves =
    match (prev, target) with
    | n1, n2 when n1 = n2 -> ""
    | 'A', '0' -> "<"
    | 'A', '1' -> "^<<"
    | 'A', '4' -> "^^<<"
    | 'A', '7' -> "^^^<<"
    | '0', 'A' -> ">"
    | '1', 'A' -> ">>v"
    | '4', 'A' -> ">>vv"
    | '7', 'A' -> ">>vvv"
    | '0', '1' -> "^<"
    | '0', '4' -> "^^<"
    | '0', '7' -> "^^^<"
    | '1', '0' -> ">v"
    | '4', '0' -> ">vv"
    | '7', '0' -> ">vvv"
    | n1, n2 -> move n1 n2
  in
  moves ^ "A" |> String.to_seq

let dpad_moves (prev, target) =
  let _dpad_moves prev target =
    match (prev, target) with
    | c1, c2 when c1 = c2 -> [ "" ]
    | 'A', '^' -> [ "<" ]
    | 'A', '>' -> [ "v" ]
    | 'A', 'v' -> [ "<v"; "v<" ]
    | 'A', '<' -> [ "v<<" ]
    | '^', 'A' -> [ ">" ]
    | '^', '>' -> [ "v>" ]
    | '^', 'v' -> [ "v" ]
    | '^', '<' -> [ "v<" ]
    | '<', 'A' -> [ ">>^" ]
    | '<', '>' -> [ ">>" ]
    | '<', 'v' -> [ ">" ]
    | '<', '^' -> [ ">^" ]
    | 'v', 'A' -> [ ">^"; "^>" ]
    | 'v', '<' -> [ "<" ]
    | 'v', '>' -> [ ">" ]
    | 'v', '^' -> [ "^" ]
    | '>', 'A' -> [ "^" ]
    | '>', '<' -> [ "<<" ]
    | '>', 'v' -> [ "<" ]
    | '>', '^' -> [ "<^"; "^<" ]
    | _ -> failwith "invalid code"
  in
  _dpad_moves prev target |> List.to_seq
  |> Seq.map (fun s -> s ^ "A")
  |> Seq.map String.to_seq

let pairwise start seq = Seq.zip (Seq.append (Seq.return start) seq) seq

let numeric s =
  String.split_on_char 'A' s |> Fun.flip List.nth 0 |> int_of_string

let solve n code =
  let memo = Fun.memo () in
  let rec moves_aux (n, (k1, k2)) =
    if n = 0 then 1
    else
      dpad_moves (k1, k2)
      |> Seq.map (fun moves ->
             moves |> pairwise 'A'
             |> Seq.map (fun p -> memo moves_aux (n - 1, p))
             |> Seq.sum)
      |> Seq.min_opt |> Option.get
  in
  let moves = memo moves_aux in
  let npad_moves =
    String.to_seq code |> pairwise 'A' |> Seq.flat_map npad_moves
  in
  npad_moves |> pairwise 'A' |> Seq.map (fun pair -> moves (n, pair)) |> Seq.sum

let _ =
  let lines = [ "140A"; "180A"; "176A"; "805A"; "638A" ] in
  lines
  |> List.parmap (fun x -> solve 2 x * numeric x)
  |> List.to_seq |> Seq.sum |> string_of_int |> print_endline;

  lines
  |> List.parmap (fun x -> solve 25 x * numeric x)
  |> List.to_seq |> Seq.sum |> string_of_int |> print_endline
