module Point = Point_2d.Make (Int)
module Point_map = Map.Make (Point)

module Part_one = struct
  type direction =
    | Horizontal
    | Vertical
    | Diagonal_left
    | Diagonal_right

  type expect =
    { direction : direction
    ; is_forward : bool
    ; symbol : char
    }

  type state =
    { expect : expect list Point_map.t
    ; count : int
    }

  let empty_state = { expect = Point_map.empty; count = 0 }

  let append pos expect state =
    let expect = Point_map.add_to_list pos expect state.expect in
    { state with expect }
  ;;

  let add_initials pos is_forward symbol state =
    let x, y = pos |> Point.to_pair in
    state
    |> append (Point.create (x + 1) y) { direction = Horizontal; is_forward; symbol }
    |> append (Point.create x (y + 1)) { direction = Vertical; is_forward; symbol }
    |> append
         (Point.create (x - 1) (y + 1))
         { direction = Diagonal_left; is_forward; symbol }
    |> append
         (Point.create (x + 1) (y + 1))
         { direction = Diagonal_right; is_forward; symbol }
  ;;

  let check_bounds pos symbol state =
    let is_forward =
      match symbol with
      | 'S' -> true
      | 'X' -> false
      | _ -> raise (Invalid_argument "Symbol isn't bound")
    in
    let is_matches expect = expect.is_forward = is_forward && expect.symbol = symbol in
    match Point_map.find_opt pos state.expect with
    | Some xs ->
      let count = xs |> List.filter is_matches |> List.length in
      { state with count = state.count + count }
    | None -> state
  ;;

  let handle_bound pos symbol state =
    let is_forward, next_symbol =
      match symbol with
      | 'X' -> true, 'M'
      | 'S' -> false, 'A'
      | _ -> raise (Invalid_argument "Symbol isn't bound")
    in
    state |> check_bounds pos symbol |> add_initials pos is_forward next_symbol
  ;;

  let add_middle pos direction is_forward symbol =
    let open Point in
    let offset =
      match direction with
      | Vertical -> create 0 1
      | Horizontal -> create 1 0
      | Diagonal_left -> create (-1) 1
      | Diagonal_right -> create 1 1
    in
    let next = pos ++ offset in
    append next { direction; is_forward; symbol }
  ;;

  let handle_middle pos symbol state =
    let is_matches expect = expect.symbol = symbol in
    let add_next state expect =
      let next =
        match symbol with
        | 'M' -> if expect.is_forward then 'A' else 'X'
        | 'A' -> if expect.is_forward then 'S' else 'M'
        | _ -> raise (Invalid_argument "Symbol isn't middle")
      in
      add_middle pos expect.direction expect.is_forward next state
    in
    match Point_map.find_opt pos state.expect with
    | Some xs -> xs |> List.filter is_matches |> List.fold_left add_next state
    | None -> state
  ;;

  let handle state (symbol, pos) =
    match symbol with
    | 'X' | 'S' -> handle_bound pos symbol state
    | 'M' | 'A' -> handle_middle pos symbol state
    | _ -> raise (Invalid_argument "Unknown symbol")
  ;;

  let count chars =
    let { count; _ } = Seq.fold_left handle empty_state chars in
    count
  ;;
end

module Part_two = struct
  module Counter = Map.Make (Int)

  type state =
    { id : int
    ; counters : int Counter.t
    ; expect : (int * char) list Point_map.t
    }

  let empty_state = { id = 0; counters = Counter.empty; expect = Point_map.empty }

  let shape = [ 2, 0; 1, 1; 0, 2; 2, 2 ] |> List.map Point.of_pair

  let with_offsets pos = shape |> List.map (fun it -> Point.( ++ ) pos it)

  let register_shape pos symbols state =
    let positions = with_offsets pos in
    let append state (pos, symbol) =
      let next = Point_map.add_to_list pos (state.id, symbol) state.expect in
      { state with expect = next }
    in
    let next = List.combine positions symbols |> List.fold_left append state in
    { next with id = next.id + 1 }
  ;;

  let register pos symbol state =
    let symbols =
      match symbol with
      | 'M' -> [ [ 'S'; 'A'; 'M'; 'S' ]; [ 'M'; 'A'; 'S'; 'S' ] ]
      | 'S' -> [ [ 'M'; 'A'; 'S'; 'M' ]; [ 'S'; 'A'; 'M'; 'M' ] ]
      | _ -> raise (Invalid_argument "Unknown symbol")
    in
    List.fold_left (fun acc it -> register_shape pos it acc) state symbols
  ;;

  let increment state id =
    let update = function
      | Some count -> Some (count + 1)
      | None -> Some 1
    in
    let next = Counter.update id update state.counters in
    { state with counters = next }
  ;;

  let check pos symbol state =
    let transform (id, expected) = if expected == symbol then Some id else None in
    match Point_map.find_opt pos state.expect with
    | Some xs -> xs |> List.filter_map transform |> List.fold_left increment state
    | None -> state
  ;;

  let handle state (symbol, position) =
    match symbol with
    | 'M' | 'S' -> state |> check position symbol |> register position symbol
    | 'A' -> state |> check position symbol
    | 'X' -> state
    | _ -> raise (Invalid_argument "Unknown symbol")
  ;;

  let complete state =
    state.counters
    |> Counter.bindings
    |> List.filter (fun (_, count) -> count = 4)
    |> List.length
  ;;

  let count chars = chars |> Seq.fold_left handle empty_state |> complete
end

let read input =
  let to_indexed_line y line =
    String.to_seq line |> Seq.mapi (fun x it -> it, Point.create x y)
  in
  let to_indexed_lines = Seq.mapi (fun y it -> to_indexed_line y it) in
  input |> Reader.lines_of |> to_indexed_lines |> Seq.concat
;;

let part_one input = input |> read |> Part_one.count |> string_of_int

let part_two input = input |> read |> Part_two.count |> string_of_int
