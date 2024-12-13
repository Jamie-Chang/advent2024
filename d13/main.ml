open Advent2024.Common

let parse_lines =
  let rec fold_lines batch lines =
    let parse_prize line =
      String.split_on_char ':' line
      |> Fun.flip List.nth 1 |> String.split_on_char ','
      |> List.map (fun e ->
             String.split_on_char '=' e |> Fun.flip List.nth 1 |> int_of_string)
      |> IntPair.of_list
    in
    let parse_button line =
      String.split_on_char ':' line
      |> Fun.flip List.nth 1 |> String.split_on_char ','
      |> List.map (fun e ->
             String.split_on_char '+' e |> Fun.flip List.nth 1 |> int_of_string)
      |> IntPair.of_list
    in
    match Seq.uncons lines with
    | Some ("", rest) -> (
        match batch with
        | [ prize; b; a ] ->
            let elem = (parse_button a, parse_button b, parse_prize prize) in
            fun () -> Seq.Cons (elem, fold_lines [] rest)
        | _ -> failwith "invalid input")
    | Some (line, rest) -> fold_lines (line :: batch) rest
    | None -> (
        match batch with
        | [ prize; b; a ] ->
            Seq.return (parse_button a, parse_button b, parse_prize prize)
        | _ -> failwith "invalid input")
  in
  fold_lines []

let solve ((ax, ay), (bx, by), (px, py)) =
  let ( /? ) a b = if a mod b = 0 then Some (a / b) else None in
  let divider = (ay * bx) - (ax * by) in
  match ((ay * px) - (ax * py)) /? divider with
  | None -> None
  | Some b -> (
      match (px - (b * bx)) /? ax with
      | None -> None
      | Some a -> Some ((a * 3) + b))

let _ =
  let filename = "inputs/d13.txt" in
  let part1 =
    In_channel.with_open_text filename @@ fun ic ->
    let machines = Seq.read_lines ic |> parse_lines in
    machines |> Seq.filter_map solve |> Seq.sum
  in
  let part2 =
    let offset = 10000000000000 in
    In_channel.with_open_text filename @@ fun ic ->
    let machines =
      Seq.read_lines ic |> parse_lines
      |> Seq.map @@ fun (a, b, (px, py)) -> (a, b, (px + offset, py + offset))
    in
    machines |> Seq.filter_map solve |> Seq.sum
  in
  (part1, part2) |> IntPair.to_string |> print_endline
