open Advent2024

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
;;

assert (blink 20 |> List.of_seq = [ 2; 0 ]);;
assert (blink 0 |> List.of_seq = [ 1 ]);;
assert (blink 1 |> List.of_seq = [ 2024 ])

let rec blink_n n seq =
  if n = 0 then seq else blink_n (n - 1) @@ Seq.flat_map blink seq
;;

assert (blink_n 25 (List.to_seq [ 125; 17 ]) |> Seq.length = 55312)

let parse raw =
  String.split_on_char ' ' raw |> List.to_seq |> Seq.map int_of_string

let part1 filename =
  In_channel.with_open_text filename In_channel.input_all
  |> parse |> blink_n 25 |> Seq.length
;;

part1 "inputs/d11.txt" |> string_of_int |> print_endline

let blink_n_memoi n v = Seq.return v |> blink_n n |> Seq.memoize

let solve items =
  let first_pass = Seq.map (blink_n_memoi 25) items in
  let cached = Hashtbl.of_seq (Seq.zip items first_pass) in
  let second_pass =
    Seq.concat first_pass
    |> Seq.map @@ fun n ->
       if Hashtbl.mem cached n then Hashtbl.find cached n
       else
         let res = blink_n_memoi 25 n in
         Hashtbl.add cached n res;
         res
  in
  let cached_size =
    Hashtbl.to_seq cached
    |> Seq.map (fun (k, v) -> (k, Seq.length v))
    |> Hashtbl.of_seq
  in
  let sizes =
    Seq.concat second_pass
    |> Seq.map @@ fun n ->
       if Hashtbl.mem cached_size n then Hashtbl.find cached_size n
       else
         let res = blink_n_memoi 25 n |> Seq.length in
         Hashtbl.add cached_size n res;
         res
  in
  Common.Seq.sum sizes

let part2 filename =
  In_channel.with_open_text filename In_channel.input_all |> parse |> solve
;;

part2 "inputs/d11.txt" |> string_of_int |> print_endline
