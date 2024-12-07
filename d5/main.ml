open Advent2024

module IntListSet = Set.Make (struct
  type t = int list

  let compare = compare
end)

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
  Common.Seq.read_lines ic
  |> Seq.take_while @@ ( <> ) ""
  |> Seq.map @@ String.split_on_char '|'
  |> Seq.map @@ List.map int_of_string
  |> IntListSet.of_seq

let parse_pages ic =
  Common.Seq.read_lines ic
  |> Seq.map @@ String.split_on_char ','
  |> Seq.map @@ List.map int_of_string

let part1 filename =
  In_channel.with_open_text filename @@ fun ic ->
  let rules = parse_rules ic in
  let pages = parse_pages ic in
  Seq.filter (check_pages rules) pages |> Seq.map middle |> Common.Seq.sum

let part2 filename =
  In_channel.with_open_text filename @@ fun ic ->
  let rules = parse_rules ic in
  let pages = parse_pages ic in
  let comparator a = function
    | b when a = b -> 0
    | b when IntListSet.mem [ a; b ] rules -> -1
    | _ -> 1
  in
  Seq.filter (Fun.negate @@ check_pages rules) pages
  |> Seq.map @@ List.sort comparator
  |> Seq.map middle |> Common.Seq.sum

let filename = "inputs/d5.txt";;
assert (part1 filename = 5732) ;;
assert (part2 filename = 4716) ;;
