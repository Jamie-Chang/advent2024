module Fun = struct
  include Fun

  let memo ?(cache_size = 1000) f =
    let cache = Hashtbl.create cache_size in
    let run v =
      match Hashtbl.find_opt cache v with
      | Some res -> res
      | None ->
          let res = f v in
          Hashtbl.add cache v res;
          res
    in
    run

  let rec recurse n f = if n = 1 then f else Fun.compose f @@ recurse (n - 1) f
end

module List = struct
  include List

  let sum = List.fold_left ( + ) 0
  let parmap f lst = Parmap.L lst |> Parmap.parmap f
end

module Seq = struct
  include Seq

  let sum = Seq.fold_left ( + ) 0

  let read_lines ic =
    let rec lines_rec ic () =
      match In_channel.input_line ic with
      | Some value -> Seq.Cons (value, lines_rec ic)
      | None -> Seq.Nil
    in
    lines_rec ic

  let window n seq =
    let rec window_rec seq () =
      match Seq.take n seq |> List.of_seq with
      | taken when List.length taken == n ->
          Seq.Cons (taken, window_rec (Seq.drop 1 seq))
      | _ -> Seq.Nil
    in

    window_rec seq

  let zip seq =
    let rec zip_rec seqs () =
      let first seq = Seq.take 1 seq |> List.of_seq in
      let take seqs = seqs |> Seq.map first in
      let drop seqs = seqs |> Seq.map @@ Seq.drop 1 in
      let column = take seqs in
      if Seq.for_all (( <> ) []) column then
        Seq.Cons (List.concat @@ List.of_seq column, zip_rec @@ drop seqs)
      else Seq.Nil
    in
    zip_rec seq

  let zip_longest seq =
    let rec zip_rec seqs () =
      let first seq = Seq.take 1 seq |> List.of_seq in
      let take seqs = seqs |> Seq.map first in
      let drop seqs = seqs |> Seq.map @@ Seq.drop 1 in
      let column = take seqs in
      let flattened = List.concat @@ List.of_seq column in
      if flattened <> [] then Seq.Cons (flattened, zip_rec @@ drop seqs)
      else Seq.Nil
    in
    zip_rec seq

  let product_n seq_of_seq =
    let product acc seq =
      Seq.product acc seq
      |> Seq.map (fun (rest, v) -> v :: rest)
      |> Seq.map List.rev
    in
    match Seq.uncons seq_of_seq with
    | Some (head, tail) ->
        Seq.fold_left product (Seq.map (fun a -> [ a ]) head) tail
    | None -> failwith "empty sequence"
end

module IntPair = struct
  type t = int * int

  let origin = (0, 0)
  let ( - ) (a, b) (c, d) = (a - c, b - d)
  let ( + ) (a, b) (c, d) = (a + c, b + d)
  let ( * ) (a, b) n = (a * n, b * n)
  let ( ~- ) (a, b) = (~-a, ~-b)

  let bound ?(start = origin) (x2, y2) (x, y) =
    let x1, y1 = start in
    x >= x1 && y >= y1 && x < x2 && y < y2

  let compare (x1, y1) (x2, y2) =
    match Stdlib.compare x1 x2 with 0 -> Stdlib.compare y1 y2 | c -> c

  let reduce (a, b) =
    let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
    let factor = gcd a b in
    (a / factor, b / factor)

  let to_string (a, b) = "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")";;
end

module Counter = struct
  module Make (Ord : Map.OrderedType) = struct
    module CounterMap = Map.Make (Ord)
    include CounterMap

    let merge =
      CounterMap.merge @@ fun _ a b ->
      match (a, b) with
      | Some a, None -> Some a
      | None, Some b -> Some b
      | Some a, Some b -> Some (a + b)
      | None, None -> Some 0

    let ( * ) n = CounterMap.map @@ ( * ) n
    let total c = CounterMap.to_seq c |> Seq.map snd |> Seq.sum

    let inc key =
      CounterMap.update key @@ function
      | Some v -> Some (v + 1)
      | None -> Some 1

    let of_value_seq = CounterMap.empty |> Seq.fold_left @@ Fun.flip inc
  end
end
