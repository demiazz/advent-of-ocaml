let chars_of input =
  let len = String.length input in
  let read idx =
    if idx >= len
    then None
    else (
      let ch = input.[idx] in
      match ch with
      | '\n' -> None
      | _ -> Some (ch, idx + 1))
  in
  Seq.unfold read 0
;;

let bytes_of input = input |> chars_of |> Seq.map Char.code

let lines_of input =
  let len = String.length input in
  let read cursor =
    if cursor >= len
    then None
    else (
      let bound =
        match String.index_from_opt input cursor '\n' with
        | Some bound -> bound
        | None -> len
      in
      let line = String.sub input cursor (bound - cursor) in
      let next = if bound < len then bound + 1 else len in
      Some (line, next))
  in
  Seq.unfold read 0
;;
