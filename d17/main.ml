open Advent2024.Common
module StrMap = Map.Make (String)

let parse_registers lines =
  let parse_line line =
    match String.split_on_char ':' line with
    | [ reg; value ] ->
        ( String.split_on_char ' ' reg |> Fun.flip List.nth 1,
          String.trim value |> int_of_string )
    | _ -> failwith "invalid line"
  in
  Seq.map parse_line lines |> StrMap.of_seq

let parse_program line =
  String.split_on_char ' ' line
  |> Fun.flip List.nth 1 |> String.split_on_char ',' |> List.map int_of_string

let parse lines =
  let registers = Seq.take_while (( <> ) "") lines |> parse_registers in
  let program =
    match Seq.uncons lines with
    | Some (program, _) -> parse_program program
    | None -> failwith "invalid program"
  in
  (registers, program)

let execute (registers, program) =
  let rec execute (registers, tape) stdout =
    let reg name = StrMap.find name registers in
    let write name value =
      registers |> StrMap.update name @@ function _ -> Some value
    in
    let combo = function
      | 0 -> 0
      | 1 -> 1
      | 2 -> 2
      | 3 -> 3
      | 4 -> reg "A"
      | 5 -> reg "B"
      | 6 -> reg "C"
      | _ -> failwith "invalid combo"
    in
    match tape with
    | 0 :: n :: rest ->
        let result =
          (reg "A" |> float_of_int) /. (2. ** (combo n |> float_of_int))
          |> int_of_float
        in
        execute (write "A" result, rest) stdout
    | 1 :: n :: rest ->
        let result = Int.logxor (reg "B") n in
        execute (write "B" result, rest) stdout
    | 2 :: n :: rest -> execute (combo n mod 8 |> write "B", rest) stdout
    | 3 :: n :: rest ->
        if reg "A" = 0 then execute (registers, rest) stdout
        else execute (registers, List.drop n program) stdout
    | 4 :: _ :: rest ->
        let result = Int.logxor (reg "B") (reg "C") in
        execute (write "B" result, rest) stdout
    | 5 :: n :: rest ->
        let result = combo n mod 8 in
        execute (registers, rest) (result :: stdout)
    | 6 :: n :: rest ->
        let result =
          (reg "A" |> float_of_int) /. (2. ** (combo n |> float_of_int))
          |> int_of_float
        in
        execute (write "B" result, rest) stdout
    | 7 :: n :: rest ->
        let result =
          (reg "A" |> float_of_int) /. (2. ** (combo n |> float_of_int))
          |> int_of_float
        in
        execute (write "C" result, rest) stdout
    | [] -> stdout
    | _ -> failwith "invalid instruction"
  in
  execute (registers, program) []
  |> List.rev |> List.map string_of_int |> String.concat ","

(*
while A <> 0
    B = A mod 8 
    B = B xor 1
    C = A >> B
    A = A >> 3
    B = B xor C
    B = B xor 6 
    out B mod 8
*)

let get_output a b =
  let a = Int.shift_left a 3 + b in
  let b = Int.logxor b 1 in
  let c = Int.shift_right a b in
  b |> Int.logxor c |> Int.logxor 6 |> Fun.flip ( mod ) 8

let rec search_digits a digits =
  let numbers = Seq.ints 0 |> Seq.take 8 in
  match digits with
  | [] -> Seq.return a
  | out :: rest ->
      let possible = numbers |> Seq.filter @@ fun n -> out = get_output a n in
      possible
      |> Seq.map (( + ) (Int.shift_left a 3))
      |> Seq.flat_map @@ Fun.flip search_digits rest

let _ =
  In_channel.with_open_text "inputs/d17.txt" @@ fun ic ->
  let computer = Seq.read_lines ic |> parse in
  execute computer |> print_endline;
  snd computer |> List.rev |> search_digits 0 |> Seq.uncons |> Option.get |> fst
  |> string_of_int |> print_endline
