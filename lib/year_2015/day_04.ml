let parse input =
  let lines = input |> Reader.lines_of |> Seq.take 1 |> List.of_seq in
  match lines with
  | [ line ] -> line
  | _ -> raise Parse.Invalid_input
;;

let encode input round =
  Printf.sprintf "%s%d" input round |> Digest.MD5.string |> Digest.MD5.to_hex
;;

let rounds_of key =
  let encode round =
    let source = Printf.sprintf "%s%d" key round in
    let hex = source |> Digest.MD5.string |> Digest.MD5.to_hex in
    hex
  in
  Seq.ints 0 |> Seq.map encode
;;

let find_round prefix input =
  let matches hex = String.starts_with ~prefix hex in
  let result = input |> parse |> rounds_of |> Seq.find_index matches in
  match result with
  | Some round -> string_of_int round
  | None -> failwith "unreachable"
;;

let part_one input = find_round "00000" input

let part_two input = find_round "000000" input
