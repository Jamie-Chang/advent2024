module StdList = List

module Fun = struct
  let ($) = Fun.compose
end

module List = struct
  let sum = StdList.fold_left ( + ) 0
  let parmap f lst = Parmap.L lst |> Parmap.parmap f
end

module Seq = struct
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
      match Seq.take n seq |> StdList.of_seq with
      | taken when StdList.length taken == n ->
          Seq.Cons (taken, window_rec (Seq.drop 1 seq))
      | _ -> Seq.Nil
    in

    window_rec seq

  let zip seq =
    let rec zip_rec seqs () =
      let first seq = Seq.take 1 seq |> StdList.of_seq in
      let take seqs = seqs |> Seq.map first in
      let drop seqs = seqs |> Seq.map @@ Seq.drop 1 in
      let column = take seqs in
      if Seq.for_all (( <> ) []) column then
        Seq.Cons (StdList.concat @@ StdList.of_seq column, zip_rec @@ drop seqs)
      else Seq.Nil
    in
    zip_rec seq

  let zip_longest seq =
    let rec zip_rec seqs () =
      let first seq = Seq.take 1 seq |> StdList.of_seq in
      let take seqs = seqs |> Seq.map first in
      let drop seqs = seqs |> Seq.map @@ Seq.drop 1 in
      let column = take seqs in
      let flattened = StdList.concat @@ StdList.of_seq column in
      if flattened <> [] then Seq.Cons (flattened, zip_rec @@ drop seqs)
      else Seq.Nil
    in
    zip_rec seq

  let product_n seq_of_seq =
    let product acc seq =
      Seq.product acc seq
      |> Seq.map (fun (rest, v) -> v :: rest)
      |> Seq.map StdList.rev
    in
    match Seq.uncons seq_of_seq with
    | Some (head, tail) ->
        Seq.fold_left product (Seq.map (fun a -> [ a ]) head) tail
    | None -> failwith "empty sequence"
end

module IntPair = struct
  let origin = (0, 0)
  let ( - ) (a, b) (c, d) = (a - c, b - d)
  let ( + ) (a, b) (c, d) = (a + c, b + d)
  let ( * ) (a, b) n = (a * n, b * n)
  let ( ~- ) (a, b) = (~-a, ~-b)
  let ( < ) (a, b) (c, d) = a < c && b < d
  let ( <= ) (a, b) (c, d) = a <= c && b <= d
  let ( > ) (a, b) (c, d) = a > c && b > d
  let ( >= ) (a, b) (c, d) = a >= c && b >= d

  let reduce (a, b) =
    let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
    let factor = gcd a b in
    (a / factor, b / factor)

  let to_string (a, b) = "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")";;

  assert (origin = (0, 0));;
  assert ((1, 2) > (0, 0));;
  assert ((1, 2) <= (1, 2));;
  assert ((1, 2) - (0, 0) = (1, 2));;
  assert ((1, 2) + (1, 1) = (2, 3));;
  assert (-(1, 1) = (-1, -1));;
  assert (reduce (5, 10) = (1, 2))
end
