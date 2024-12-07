open Advent2024

let read_matrix filename =
  In_channel.with_open_text filename (fun ic ->
      Common.Seq.read_lines ic
      |> Seq.map @@ Fun.compose List.of_seq String.to_seq
      |> List.of_seq)

let columns_of matrix = Common.Seq.zip matrix |> Seq.map List.to_seq

let pad n seq =
  let padding = Seq.repeat None |> Seq.take n in
  let seq = Seq.map (fun v -> Some v) seq in
  List.to_seq [ padding; seq ] |> Seq.concat

let d1_of matrix =
  Seq.mapi pad matrix |> Common.Seq.zip_longest
  |> Seq.map (Fun.compose List.to_seq @@ List.filter_map Fun.id) 

let d2_of matrix =
  let len = Seq.length matrix in
  let pad i = pad @@ (len - 1 - i) in
  Seq.mapi pad matrix |> Common.Seq.zip_longest
  |> Seq.map (Fun.compose List.to_seq @@ List.filter_map Fun.id) 

let join = Fun.compose String.of_seq List.to_seq

let count_word line =
  line |> Common.Seq.window 4 |> Seq.map join
  |> Seq.filter (function "XMAS" -> true | "SAMX" -> true | _ -> false)
  |> Seq.length

let part1 filename =
  let matrix = read_matrix filename |> List.to_seq |> Seq.map List.to_seq in
  let rows = matrix in
  let cols = columns_of matrix in
  let d1 = d1_of matrix in
  let d2 = d2_of matrix in
  let count lines = lines |> Seq.map count_word |> Common.Seq.sum in
  count rows + count cols + count d1 + count d2
;;

part1 "inputs/d4.txt" |> string_of_int |> print_endline

let has_cross matrix =
  match matrix with
  | [ c00 :: _ :: c02 :: _; _ :: c11 :: _ :: _; c20 :: _ :: c22 :: _ ] ->
      let d1 = join [ c00; c11; c22 ] in
      let d2 = join [ c02; c11; c20 ] in
      (d1 = "MAS" || d1 = "SAM") && (d2 = "MAS" || d2 = "SAM")
  | _ -> failwith "unexpected shape"

let part2 filename =
  let horizontal_window matrix =
    List.to_seq matrix |> Seq.map List.to_seq
    |> Seq.map (Common.Seq.window 3)
    |> Common.Seq.zip
  in

  read_matrix filename |> List.to_seq |> Common.Seq.window 3
  |> Seq.map horizontal_window
  |> Seq.map @@ Seq.filter has_cross
  |> Seq.map Seq.length |> Common.Seq.sum
;;

part2 "inputs/d4.txt" |> print_int |> print_newline
