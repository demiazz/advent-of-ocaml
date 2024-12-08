module Common = struct
  let dispenser_of item_of string =
    string |> Knife.String.indices |> Seq.map item_of |> Seq.to_dispenser
  ;;

  let count_of is_nice input =
    let validate string = Some (is_nice string) in
    let count results = results |> Seq.filter Fun.id |> Seq.length in
    input |> Parse.lines_of validate |> count |> string_of_int
  ;;
end

module Part_one = struct
  let dispenser_of string =
    let pair_of = function
      | 0 -> None, string.[0]
      | idx -> Some string.[idx - 1], string.[idx]
    in
    string |> Common.dispenser_of pair_of
  ;;

  type state =
    { vowels_count : int
    ; has_twice : bool
    }

  let is_bad_pair = function
    | Some 'a', 'b' | Some 'c', 'd' | Some 'p', 'q' | Some 'x', 'y' -> true
    | _ -> false
  ;;

  let is_vowel = function
    | 'a' | 'e' | 'i' | 'o' | 'u' -> true
    | _ -> false
  ;;

  let detect_vowel pair state =
    if is_vowel (snd pair)
    then { state with vowels_count = state.vowels_count + 1 }
    else state
  ;;

  let is_twice = function
    | Some x, y when x = y -> true
    | _ -> false
  ;;

  let detect_twice pair state =
    if state.has_twice then state else { state with has_twice = is_twice pair }
  ;;

  let is_nice string =
    let next = dispenser_of string in
    let rec validate state =
      match next () with
      | Some pair ->
        if is_bad_pair pair
        then false
        else state |> detect_vowel pair |> detect_twice pair |> validate
      | None -> state.vowels_count >= 3 && state.has_twice
    in
    let initial = { vowels_count = 0; has_twice = false } in
    validate initial
  ;;
end

module Part_two = struct
  module PairSet = Set.Make (struct
      type t = char * char

      let compare = Stdlib.compare
    end)

  let dispenser_of string =
    let triple_of = function
      | 0 -> None, None, string.[0]
      | 1 -> None, Some string.[0], string.[1]
      | idx -> Some string.[idx - 2], Some string.[idx - 1], string.[idx]
    in
    string |> Common.dispenser_of triple_of
  ;;

  type state =
    { has_char_repeat : bool
    ; has_pair_repeat : bool
    ; protect_from_overlap : bool
    ; seen : PairSet.t
    }

  let detect_char_repeat triple state =
    if state.has_char_repeat
    then state
    else (
      match triple with
      | Some x, _, y when x = y -> { state with has_char_repeat = true }
      | _ -> state)
  ;;

  let detect_pair_repeat triple state =
    if state.has_pair_repeat
    then state
    else (
      match triple with
      | _, Some x, y ->
        let is_repeat =
          if x = y && state.protect_from_overlap
          then false
          else PairSet.mem (x, y) state.seen
        in
        if is_repeat
        then { state with has_pair_repeat = true }
        else
          { state with
            protect_from_overlap = x = y && not state.protect_from_overlap
          ; seen = PairSet.add (x, y) state.seen
          }
      | _ ->
        if state.protect_from_overlap
        then { state with protect_from_overlap = false }
        else state)
  ;;

  let is_nice string =
    let next = dispenser_of string in
    let rec validate state =
      match next () with
      | Some triple ->
        let next_state =
          state |> detect_char_repeat triple |> detect_pair_repeat triple
        in
        if next_state.has_char_repeat && next_state.has_pair_repeat
        then true
        else validate next_state
      | None -> false
    in
    let initial =
      { has_char_repeat = false
      ; has_pair_repeat = false
      ; protect_from_overlap = false
      ; seen = PairSet.empty
      }
    in
    validate initial
  ;;
end

let part_one input = Common.count_of Part_one.is_nice input

let part_two input = Common.count_of Part_two.is_nice input
