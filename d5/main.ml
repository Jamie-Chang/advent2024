open Advent2024

module IntListSet = Set.Make (struct
  type t = int list

  let compare = compare
end)

let read_lines ic =
  let rec lines_rec ic () =
    match In_channel.input_line ic with
    | Some value -> Seq.Cons (value, lines_rec ic)
    | None -> Seq.Nil
  in
  lines_rec ic

let rec check_pages rules pages =
  match pages with
  | a :: b :: rest ->
      if IntListSet.mem [ a; b ] rules then check_pages rules (b :: rest)
      else false
  | _ -> true

let middle lst =
  let n = List.length lst / 2 in
  List.nth lst n

let parse_rules ic =
  read_lines ic
  |> Seq.take_while @@ ( <> ) ""
  |> Seq.map @@ String.split_on_char '|'
  |> Seq.map @@ List.map int_of_string
  |> IntListSet.of_seq

let parse_pages ic =
  read_lines ic
  |> Seq.map @@ String.split_on_char ','
  |> Seq.map @@ List.map int_of_string

let part1 filename =
  In_channel.with_open_text filename (fun ic ->
      let rules = parse_rules ic in
      let pages = parse_pages ic in
      Seq.filter (check_pages rules) pages |> Seq.map middle |> Common.Seq.sum)

let part2 filename =
  In_channel.with_open_text filename (fun ic ->
      let rules = parse_rules ic in
      let pages = parse_pages ic in
      let comparator a = function
        | b when a = b -> 0
        | b when IntListSet.mem [ a; b ] rules -> -1
        | _ -> 1
      in
      Seq.filter (Fun.negate @@ check_pages rules) pages
      |> Seq.map @@ List.sort comparator
      |> Seq.map middle |> Common.Seq.sum)

let filename = "inputs/d5.txt";;

part1 filename |> print_int |> print_newline;;
part2 filename |> print_int |> print_newline
