let parse input =
  let process line =
    match String.index_opt line ' ' with
    | Some index ->
      (match String.sub line index 3 with
       | "   " ->
         let length = String.length line in
         let left = String.sub line 0 index |> int_of_string in
         let right = String.sub line (index + 3) (length - index - 3) |> int_of_string in
         Some (left, right)
       | _ -> None)
    | None -> None
  in
  input |> Parse.lines_of process
;;

(* NOTE: For part one we can not read sequence of pairs, and then convert it
   to list and sort. I think, we can use sorting tree here, and write
   to two tries in parallel, and then just count result. *)
module Part_one = struct
  let split =
    let fold (ls, rs) (l, r) = l :: ls, r :: rs in
    Seq.fold_left fold ([], [])
  ;;

  let sort (left, right) =
    let sort = List.fast_sort Stdlib.compare in
    sort left, sort right
  ;;

  let sum (left, right) =
    let diff (l, r) = Stdlib.abs (l - r) in
    let pairs = Seq.zip (List.to_seq left) (List.to_seq right) in
    pairs |> Seq.map diff |> Knife.Seq.sum
  ;;

  let distance input = input |> parse |> split |> sort |> sum |> string_of_int
end

module Part_two = struct
  module Map = Map.Make (Int)

  let update value =
    let increase = function
      | Some count -> Some (count + 1)
      | None -> Some 1
    in
    Map.update value increase
  ;;

  let count =
    let fold (lc, rc) (l, r) = update l lc, update r rc in
    Seq.fold_left fold (Map.empty, Map.empty)
  ;;

  let similarity (lc, rc) =
    let fold total (value, count) =
      match Map.find_opt value rc with
      | Some frequency -> total + (value * count * frequency)
      | None -> total
    in
    lc |> Map.bindings |> List.fold_left fold 0
  ;;

  let score input = input |> parse |> count |> similarity |> string_of_int
end

let part_one = Part_one.distance

let part_two = Part_two.score
