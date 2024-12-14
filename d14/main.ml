open Advent2024.Common
module IntCounter = Counter.Make (Int)
module IntPairSet = Set.Make (IntPair)

let parse_line line =
  let to_pair = function [ x; y ] -> (x, y) | _ -> failwith "not pair" in
  let parse_pair s =
    String.split_on_char ',' s |> List.map int_of_string |> to_pair
  in
  let aux =
    String.split_on_char ' ' line
    |> List.map @@ fun s ->
       String.split_on_char '=' s |> Fun.flip List.nth 1 |> parse_pair
  in
  to_pair aux

let rec move (x_bound, y_bound) (p, v) () =
  let ( % ) x y = match x mod y with n when n < 0 -> n + y | n -> n in
  let open IntPair in
  let x, y = p + v in
  let x, y = (x % x_bound, y % y_bound) in
  Seq.Cons (p, move (x_bound, y_bound) ((x, y), v))

let dimensions = (101, 103)

let part1 filename =
  In_channel.with_open_text filename @@ fun ic ->
  let positions =
    Seq.read_lines ic |> Seq.map parse_line
    |> Seq.map @@ fun robot -> move dimensions robot |> Fun.flip Seq.nth 100
  in
  (* let _ = Seq.map IntPair.to_string positions |> Seq.iter print_endline in *)
  let quadrants =
    [
      IntPair.(bound (dimensions / 2));
      IntPair.bound
        ~start:((fst dimensions / 2) + 1, 0)
        (fst dimensions, snd dimensions / 2);
      IntPair.bound
        ~start:(0, (snd dimensions / 2) + 1)
        (fst dimensions / 2, snd dimensions);
      IntPair.bound ~start:IntPair.((dimensions / 2) + (1, 1)) dimensions;
    ]
  in
  let qs =
    positions
    |> Seq.filter_map @@ fun p -> List.find_index (fun c -> c p) quadrants
  in
  IntCounter.of_value_seq qs |> IntCounter.to_seq |> Seq.map snd
  |> Seq.fold_left ( * ) 1

let range lim = Seq.ints 0 |> Seq.take lim

let print_map (x_lim, y_lim) positions =
  let positions = IntPairSet.of_list positions in
  range y_lim
  |> Seq.iter @@ fun y ->
     (range x_lim
     |> Seq.iter @@ fun x ->
        if IntPairSet.mem (x, y) positions then print_char '#'
        else print_char ' ');
     print_newline ()

let seq_i = Seq.mapi @@ fun i v -> (i, v)

let _print_maps n filename =
  In_channel.with_open_text filename @@ fun ic ->
  let robot_positions =
    Seq.read_lines ic |> Seq.map parse_line
    |> Seq.map @@ fun robot -> move dimensions robot
  in
  Seq.memoize robot_positions
  |> Seq.zip |> seq_i
  |> Seq.take n
  |> Seq.iter @@ fun (i, positions) ->
     print_endline ("=============" ^ string_of_int i ^ "=============");
     print_map dimensions positions

let part2 filename =
  In_channel.with_open_text filename @@ fun ic ->
  let positions =
    Seq.read_lines ic |> Seq.map parse_line
    |> Seq.map @@ fun robot -> move dimensions robot |> Fun.flip Seq.nth 7286
  in
  print_map dimensions @@ List.of_seq positions

let _ =
  part1 "inputs/d14.txt" |> string_of_int |> print_endline;
  part2 "inputs/d14.txt"
