open Advent2024.Common
module IntCounter = Counter.Make (Int)

let blink num =
  if num = 0 then Seq.return 1
  else
    let digits = string_of_int num in
    let length = String.length digits in
    if length mod 2 = 0 then
      [
        String.sub digits 0 (length / 2) |> int_of_string;
        String.sub digits (length / 2) (length / 2) |> int_of_string;
      ]
      |> List.to_seq
    else Seq.return @@ (num * 2024)

let solve iters stones =
  let stones = IntCounter.of_value_seq stones in
  let blink_one = Fun.memo @@ Fun.compose IntCounter.of_value_seq blink in
  let blink_all stones =
    IntCounter.to_seq stones
    |> Seq.map (fun (k, v) -> IntCounter.(v * blink_one k))
    |> Seq.fold_left IntCounter.merge IntCounter.empty
  in
  Fun.recurse iters blink_all stones |> IntCounter.total

let parse a =
  a |> String.split_on_char ' ' |> List.to_seq |> Seq.map int_of_string

let _ =
  let file = "inputs/d11.txt" in
  let stones = In_channel.with_open_text file In_channel.input_all |> parse in
  let result = (solve 25 stones, solve 75 stones) in
  IntPair.to_string result |> print_endline
