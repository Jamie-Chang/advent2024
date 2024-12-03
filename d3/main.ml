open Iter

let read_file file = In_channel.with_open_bin file In_channel.input_all

let mul_expression =
  Re.(
    seq [ str "mul("; group (rep1 digit); str ","; group (rep digit); str ")" ])

let regex = Re.compile mul_expression

let operations content =
  Re.Seq.all regex content |> of_seq
  |> map (fun group ->
         ( Re.Group.get group 1 |> int_of_string,
           Re.Group.get group 2 |> int_of_string ))

let part1 filename =
  read_file filename |> operations |> map (fun (x, y) -> x * y) |> sum
;;

part1 "inputs/d3.txt" |> string_of_int |> print_endline

type operation = Mul of int * int | Do | Dont
type state = Enabled | Disabled

let regex = Re.(compile (alt [ str "don't"; str "do"; mul_expression ]))

let parse_group group =
  let op_string = Re.Group.get group 0 in
  if String.starts_with ~prefix:"mul" op_string then
    Mul
      ( Re.Group.get group 1 |> int_of_string,
        Re.Group.get group 2 |> int_of_string )
  else if op_string = "don't" then Dont
  else Do

let get_ops text = Re.Seq.all regex text |> of_seq |> map parse_group

let run_ops ops =
  let run_op (total, state) op =
    match (state, op) with
    | _, Do -> (total, Enabled)
    | _, Dont -> (total, Disabled)
    | Disabled, _ -> (total, Disabled)
    | Enabled, Mul (x, y) -> (total + (x * y), Enabled)
  in
  fold run_op (0, Enabled) ops

let part2 filename =
  read_file filename |> get_ops |> run_ops |> fun (total, _) -> total
;;

part2 "inputs/d3.txt" |> string_of_int |> print_endline
