open Advent2024.Common

module IntListMap = Map.Make (struct
  type t = int list

  let compare = List.compare Int.compare
end)

let mix a b = Int.logxor a b
let prune n = n mod 16777216

let next n =
  let n = n * 64 |> mix n |> prune in
  let n = n / 32 |> mix n |> prune in
  n * 2048 |> mix n |> prune

let rec sequence n () = Seq.Cons (n, sequence (next n))
let _test = assert (next 123 = 15887950)

let diff seq =
  Stdlib.Seq.zip (Seq.drop 1 seq) seq |> Seq.map (fun (a, b) -> a - b)

let price_changes seq =
  let prices = Seq.map (fun x -> x mod 10) seq in
  let price_change_seq = prices |> diff |> Seq.window 4 in
  let prices = Seq.drop 4 prices in
  Stdlib.Seq.zip price_change_seq prices

let fold_price_changes seq =
  Seq.fold_left
    (fun prices (price_change, price) ->
      IntListMap.update price_change
        (function None -> Some price | other -> other)
        prices)
    IntListMap.empty seq

let fold_buyer prices buyer =
  let merge _ price1 price2 =
    match (price1, price2) with
    | Some p1, None -> Some p1
    | None, Some p2 -> Some p2
    | Some p1, Some p2 -> Some (p1 + p2)
    | _ -> failwith "can't happen"
  in
  IntListMap.merge merge prices buyer

let part1 filename =
  In_channel.with_open_text filename @@ fun ic ->
  Seq.read_lines ic |> Seq.map int_of_string |> Seq.map sequence
  |> Seq.map (Fun.flip Seq.nth 2000)
  |> Seq.sum

let part2 filename =
  let get_price_changes n =
    sequence n |> Seq.take 2001 |> price_changes |> fold_price_changes
  in
  In_channel.with_open_text filename @@ fun ic ->
  Seq.read_lines ic |> Seq.map int_of_string |> List.of_seq
  |> List.parmap get_price_changes
  |> List.fold_left fold_buyer IntListMap.empty
  |> IntListMap.to_seq |> Seq.map snd |> Seq.max_opt |> Option.get

let _ =
  part1 "inputs/d22.txt" |> string_of_int |> print_endline;
  part2 "inputs/d22.txt" |> string_of_int |> print_endline
