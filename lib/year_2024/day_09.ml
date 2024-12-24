type blocks =
  | File of
      { id : int
      ; size : int
      }
  | Free of { size : int }

let get_digit = function
  | '0' .. '9' as raw -> Some (int_of_char raw - 48)
  | _ -> None
;;

let parse input =
  let aux index size =
    if index mod 2 = 0 then File { id = index / 2; size } else Free { size }
  in
  input |> Parse.chars_of get_digit |> Seq.mapi aux
;;

module Part_one = struct
  let rearrange blocks =
    let layout = Array.of_seq blocks in
    let bound = Array.length layout - 1 in
    let rec fill acc lidx ridx =
      if lidx >= bound || ridx <= 0 || lidx >= ridx
      then acc, lidx
      else (
        match layout.(lidx) with
        | File file -> fill (File file :: acc) (lidx + 1) ridx
        | Free free ->
          (match layout.(ridx) with
           | Free _ -> fill acc lidx (ridx - 1)
           | File file ->
             if file.size = free.size
             then (
               layout.(ridx) <- File { file with size = 0 };
               fill (File file :: acc) (lidx + 1) (ridx - 1))
             else if file.size > free.size
             then (
               layout.(ridx) <- File { file with size = file.size - free.size };
               fill (File { id = file.id; size = free.size } :: acc) (lidx + 1) ridx)
             else (
               layout.(lidx) <- Free { size = free.size - file.size };
               layout.(ridx) <- File { file with size = 0 };
               fill (File file :: acc) lidx (ridx - 1))))
    in
    let rec squash acc idx =
      if idx > bound
      then acc
      else (
        match layout.(idx) with
        | File file -> squash (File file :: acc) (idx + 1)
        | Free _ -> squash acc (idx + 1))
    in
    let filled, idx = fill [] 0 bound in
    squash filled idx |> List.rev
  ;;

  let checksum =
    let rec aux (sum, idx) = function
      | [] -> sum
      | Free _ :: xs -> aux (sum, idx) xs
      | File file :: xs ->
        if file.size = 0
        then aux (sum, idx) xs
        else
          aux
            (sum + (file.id * idx), idx + 1)
            (File { file with size = file.size - 1 } :: xs)
    in
    aux (0, 0)
  ;;
end

module Part_two = struct
  module Map = Map.Make (Int)

  let expand blocks =
    let aux block = [ block ] in
    blocks |> Seq.map aux |> Array.of_seq
  ;;

  let find_free_index layout bound limit =
    let idx = ref 0 in
    let target = ref None in
    while !idx < bound && Option.is_none !target do
      match layout.(!idx) with
      | Free { size } :: _ -> if size >= limit then target := Some !idx else incr idx
      | _ -> incr idx
    done;
    !target
  ;;

  let rearrange blocks =
    let layout = expand blocks in
    let append dest id size =
      match layout.(dest) with
      | Free free :: rest ->
        if free.size > size
        then (
          let free = Free { size = free.size - size } in
          let file = File { id; size } in
          layout.(dest) <- free :: file :: rest)
        else (
          let file = File { id; size } in
          layout.(dest) <- file :: rest)
      | _ -> ()
    in
    let remove src size = layout.(src) <- [ Free { size } ] in
    let replace src dest id size =
      remove src size;
      append dest id size
    in
    for src = Array.length layout - 1 downto 0 do
      match layout.(src) with
      | [ File { id; size } ] ->
        (match find_free_index layout src size with
         | Some dest -> replace src dest id size
         | None -> ())
      | _ -> ()
    done;
    layout |> Array.to_list |> List.map List.rev |> List.flatten
  ;;

  let checksum =
    let rec aux (sum, idx) = function
      | [] -> sum
      | Free { size } :: xs -> aux (sum, idx + size) xs
      | File file :: xs ->
        if file.size = 0
        then aux (sum, idx) xs
        else
          aux
            (sum + (file.id * idx), idx + 1)
            (File { file with size = file.size - 1 } :: xs)
    in
    aux (0, 0)
  ;;
end

let part_one input =
  input |> parse |> Part_one.rearrange |> Part_one.checksum |> string_of_int
;;

let part_two input =
  input |> parse |> Part_two.rearrange |> Part_two.checksum |> string_of_int
;;
