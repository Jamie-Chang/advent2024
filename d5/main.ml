(* let read_file filename = In_channel.with_open_text filename In_channel.input_lines *)

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
  |> Seq.take_while (fun l -> l <> "")
  |> Seq.map (fun s -> String.split_on_char '|' s |> List.map int_of_string)
  |> IntListSet.of_seq

let parse_pages ic =
  read_lines ic
  |> Seq.map (fun l -> String.split_on_char ',' l |> List.map int_of_string)

let part1 filename =
  In_channel.with_open_text filename (fun ic ->
      let rules = parse_rules ic in
      let pages = parse_pages ic in
      Seq.filter (check_pages rules) pages
      |> Seq.map middle |> Seq.fold_left ( + ) 0)

let part2 filename =
  In_channel.with_open_text filename (fun ic ->
      let rules = parse_rules ic in
      let pages = parse_pages ic in
      Seq.filter (fun a -> not (check_pages rules a)) pages
      |> Seq.map
           (List.sort (fun a b ->
                match (a, b) with
                | a, b when a = b -> 0
                | a, b when IntListSet.mem [ a; b ] rules -> -1
                | _ -> 1))
      |> Seq.map middle |> Seq.fold_left ( + ) 0)

let filename = "inputs/d5.txt";;

part1 filename |> print_int |> print_newline;;
part2 filename |> print_int |> print_newline
