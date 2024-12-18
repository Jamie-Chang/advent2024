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

let _ =
  In_channel.with_open_text "inputs/d17.txt" @@ fun ic ->
  let computer = Seq.read_lines ic |> parse in
  execute computer |> print_endline
