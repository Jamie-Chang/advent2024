open Advent2024.Common
module StringSet = Set.Make (String)

let parse_lines lines =
  let table = Hashtbl.create 3000 in
  let _ =
    Seq.map (String.split_on_char '-') lines
    |> Seq.iter @@ function
       | [ s; f ] -> (
           (match Hashtbl.find_opt table s with
           | None -> Hashtbl.add table s (StringSet.add f StringSet.empty)
           | Some set -> Hashtbl.replace table s (StringSet.add f set));
           match Hashtbl.find_opt table f with
           | None -> Hashtbl.add table f (StringSet.add s StringSet.empty)
           | Some set -> Hashtbl.replace table f (StringSet.add s set))
       | _ -> failwith "error"
  in
  table

let get_connected table computer set =
  StringSet.fold
    (fun c (remaining, pairs) ->
      let pairs =
        Hashtbl.find table c |> StringSet.inter remaining |> StringSet.to_seq
        |> Seq.map (fun o -> [ computer; c; o ])
        |> Seq.append pairs
      in
      (StringSet.remove c remaining, pairs))
    set (set, Seq.empty)
  |> snd

let get_connected_graph table computer set =
  let rec aux remaining graph =
    if StringSet.is_empty remaining then Seq.return graph
    else
      StringSet.to_seq remaining
      |> Seq.scan
           (fun (seen, _) computer ->
             let new_remaining =
               Hashtbl.find table computer |> Fun.flip StringSet.diff seen |> StringSet.inter remaining
             in
             ( StringSet.add computer seen,
               aux new_remaining (StringSet.add computer graph) ))
           (StringSet.empty, Seq.empty)
      |> Seq.flat_map snd
  in
  aux set StringSet.(add computer empty)

let part1 filename =
  let table =
    In_channel.with_open_text filename @@ fun ic ->
    Seq.read_lines ic |> parse_lines
  in
  Hashtbl.to_seq table
  |> Seq.filter (fun (key, _) -> String.starts_with ~prefix:"t" key)
  |> Seq.scan
       (fun (seen, _) (key, computers) ->
         ( StringSet.add key seen,
           get_connected table key (StringSet.diff computers seen) ))
       (StringSet.empty, Seq.empty)
  |> Seq.flat_map snd |> Seq.length

let max f default seq =
  Seq.fold_left
    (fun acc element -> if f element > f acc then element else acc)
    default seq

let part2 filename =
  let table =
    In_channel.with_open_text filename @@ fun ic ->
    Seq.read_lines ic |> parse_lines
  in
  Hashtbl.to_seq table
  |> Seq.filter (fun (key, _) -> String.starts_with ~prefix:"t" key)
  |> Seq.scan
       (fun (seen, _) (key, computers) ->
         ( StringSet.add key seen,
           get_connected_graph table key (StringSet.diff computers seen) ))
       (StringSet.empty, Seq.empty)
  |> Seq.flat_map snd
  |> max (fun s -> StringSet.to_seq s |> Seq.length) StringSet.empty
  |> StringSet.to_list |> List.sort compare |> String.concat ","

let _ =
  part1 "inputs/d23.txt" |> string_of_int |> print_endline;
  part2 "inputs/d23.txt" |> print_endline
