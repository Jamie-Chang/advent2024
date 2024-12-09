open Advent2024

let parse raw =
  String.to_seq raw |> Seq.map int_of_char |> Seq.map @@ fun a -> a - 48
;;

let _seq_mapi_tuple = Seq.mapi @@ fun i x -> (i, x)

let _pick odd seq =
  _seq_mapi_tuple seq
  |> Seq.filter_map @@ fun (i, x) -> if i mod 2 = odd then Some x else None

let fill file_stack free_slot =
  let rec fill_aux (file_stack, result_stack) (free_pos, free_cap) =
    match file_stack with
    | (file_pos, file_cap) :: file_stack when free_pos >= file_pos ->
        ((file_pos, file_cap) :: file_stack, result_stack)
    | (file_pos, file_cap) :: file_stack ->
        if free_cap > file_cap then
          fill_aux
            (file_stack, (file_pos, file_cap) :: result_stack)
            (free_pos, free_cap - file_cap)
        else if free_cap = file_cap then
          (file_stack, (file_pos, file_cap) :: result_stack)
        else
          ( (file_pos, file_cap - free_cap) :: file_stack,
            (file_pos, free_cap) :: result_stack )
    | [] -> ([], result_stack)
  in
  fill_aux (file_stack, []) free_slot |> fun (file_stack, result_stack) ->
  (file_stack, List.rev result_stack)

let combine files filled =
  let filled = Seq.append filled @@ Seq.repeat [] in
  Seq.map2 (fun a b -> a :: b) files filled |> Seq.flat_map List.to_seq
;;

assert (
  combine (List.to_seq [ 'a'; 'b' ]) (List.to_seq [ [ 'c' ] ])
  |> List.of_seq = [ 'a'; 'c'; 'b' ])

let checksum files =
  let sum_ints start n = Seq.ints start |> Seq.take n |> Common.Seq.sum in
  let totals =
    0 |> Seq.scan @@ fun total_count (_, count) -> total_count + count
  in
  let agg files = Seq.zip files @@ totals files in
  let check_sums =
    agg files
    |> Seq.map @@ fun ((file_no, count), total_count) ->
       file_no * sum_ints total_count count
  in
  Common.Seq.sum check_sums

let part1 filename =
  let raw =
    In_channel.with_open_text filename @@ fun ic -> In_channel.input_all ic
  in
  let file_stack =
    parse raw |> _pick 0 |> _seq_mapi_tuple |> List.of_seq |> List.rev
  in
  let free_slots = parse raw |> _pick 1 |> _seq_mapi_tuple in
  let rec fill_all (file_stack, result_stack) free_slots =
    if List.length result_stack >= List.length file_stack then
      (file_stack, result_stack)
    else
      match Seq.uncons free_slots with
      | None -> (file_stack, result_stack)
      | Some (free_slot, free_slots) ->
          let file_stack, results = fill file_stack free_slot in
          fill_all (file_stack, results :: result_stack) free_slots
  in

  let files_rev, filled_rev = fill_all (file_stack, []) free_slots in
  let files, filled =
    (List.to_seq (List.rev files_rev), List.to_seq (List.rev filled_rev))
  in
  combine files filled |> checksum

let fill_v2 file_stack slot =
  let rec fill_v2_aux (file_stack, result_stack, skipped_files)
      (slot_pos, slot_size) =
    if slot_size = 0 then (file_stack, result_stack, skipped_files)
    else
      match file_stack with
      | (0, size) :: file_stack ->
          fill_v2_aux
            (file_stack, result_stack, (0, size) :: skipped_files)
            (slot_pos, slot_size)
      | (file, size) :: file_stack when slot_pos >= file ->
          ( (file, size) :: file_stack,
            (0, slot_size) :: result_stack,
            skipped_files )
      | (file, size) :: file_stack when slot_size >= size ->
          fill_v2_aux
            ( file_stack,
              (file, size) :: result_stack,
              (0, size) :: skipped_files )
            (slot_pos, slot_size - size)
      | file :: file_stack ->
          fill_v2_aux
            (file_stack, result_stack, file :: skipped_files)
            (slot_pos, slot_size)
      | [] -> (file_stack, (0, slot_size) :: result_stack, skipped_files)
  in
  let file_stack, result_stack, skipped_files =
    fill_v2_aux (file_stack, [], []) slot
  in
  (List.rev skipped_files @ file_stack, List.rev result_stack)

let _debug_result seq =
  seq
  |> Seq.map Common.IntPair.to_string
  |> List.of_seq |> String.concat " " |> print_endline

let _print_result seq = seq |> checksum |> string_of_int |> print_endline

let part2 filename =
  let raw =
    In_channel.with_open_text filename @@ fun ic -> In_channel.input_all ic
  in
  let file_stack =
    parse raw |> _pick 0 |> _seq_mapi_tuple |> List.of_seq |> List.rev
  in
  let free_slots = parse raw |> _pick 1 |> _seq_mapi_tuple in
  let rec fill_all (file_stack, result_stack) free_slots =
    if List.length result_stack >= List.length file_stack then
      (file_stack, result_stack)
    else
      match Seq.uncons free_slots with
      | None -> (file_stack, result_stack)
      | Some (free_slot, free_slots) ->
          let file_stack, results = fill_v2 file_stack free_slot in
          fill_all (file_stack, results :: result_stack) free_slots
  in

  let files_rev, filled_rev = fill_all (file_stack, []) free_slots in
  let files, filled =
    (List.to_seq (List.rev files_rev), List.to_seq (List.rev filled_rev))
  in
  combine files filled

let _filename = "inputs/d9.txt";;

part1 _filename |> string_of_int |> print_endline;;
part2 _filename |> _print_result
