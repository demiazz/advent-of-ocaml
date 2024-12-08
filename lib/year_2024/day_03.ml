module State : sig
  type 'a t

  type 'a action =
    | Reset
    | Flush
    | Enable
    | Disable
    | Write_left
    | Write_right
    | Next of 'a

  val create : bool -> 'a -> 'a t

  val process : 'a action -> char -> 'a t -> 'a t

  val expected : 'a t -> 'a

  val sum : 'a t -> int

  val is_toggleable : 'a t -> bool
end = struct
  type 'a t =
    { sum : int
    ; is_toggleable : bool
    ; is_enabled : bool
    ; left : Buffer.t
    ; right : Buffer.t
    ; initial : 'a
    ; expected : 'a
    }

  type 'a action =
    | Reset
    | Flush
    | Enable
    | Disable
    | Write_left
    | Write_right
    | Next of 'a

  let create is_toggleable initial =
    { sum = 0
    ; is_toggleable
    ; is_enabled = true
    ; left = Buffer.create 0
    ; right = Buffer.create 0
    ; initial
    ; expected = initial
    }
  ;;

  let reset state =
    Buffer.reset state.left;
    Buffer.reset state.right;
    { state with expected = state.initial }
  ;;

  let int_of_buffer buffer = Buffer.contents buffer |> int_of_string

  let flush state =
    if state.is_enabled
    then (
      let left = int_of_buffer state.left in
      let right = int_of_buffer state.right in
      let sum = state.sum + (left * right) in
      { (reset state) with sum })
    else reset state
  ;;

  let toggle is_enabled state = { state with is_enabled; expected = state.initial }

  let write_left ch state =
    Buffer.add_char state.left ch;
    state
  ;;

  let write_right ch state =
    Buffer.add_char state.right ch;
    state
  ;;

  let next expected state = { state with expected }

  let process action actual state =
    let transform =
      match action with
      | Reset -> reset
      | Flush -> flush
      | Enable -> toggle true
      | Disable -> toggle false
      | Write_left -> write_left actual
      | Write_right -> write_right actual
      | Next expected -> next expected
    in
    transform state
  ;;

  let expected { expected; _ } = expected

  let sum { sum; _ } = sum

  let is_toggleable { is_toggleable; _ } = is_toggleable
end

module Parser : sig
  type expected =
    | Begin
    | Mul of char
    | Left_number
    | Right_number
    | Toggle
    | Enable_of_disable
    | Enable
    | Disable of char

  val initial : expected

  val parse : expected State.t -> char -> expected State.action
end = struct
  type expected =
    | Begin
    | Mul of char
    | Left_number
    | Right_number
    | Toggle
    | Enable_of_disable
    | Enable
    | Disable of char

  let handle_begin is_toggleable = function
    | 'm' -> State.Next (Mul 'u')
    | 'd' -> if is_toggleable then State.Next Toggle else State.Reset
    | _ -> Reset
  ;;

  let handle_mul expected actual =
    let handle_correct = function
      | 'u' -> State.Next (Mul 'l')
      | 'l' -> State.Next (Mul '(')
      | '(' -> State.Next Left_number
      | _ -> State.Reset
    in
    if expected = actual then handle_correct actual else State.Reset
  ;;

  let handle_left = function
    | ',' -> State.Next Right_number
    | '0' .. '9' -> State.Write_left
    | _ -> State.Reset
  ;;

  let handle_right = function
    | ')' -> State.Flush
    | '0' .. '9' -> State.Write_right
    | _ -> State.Reset
  ;;

  let handle_toggle = function
    | 'o' -> State.Next Enable_of_disable
    | _ -> State.Reset
  ;;

  let handle_enable_or_disable = function
    | '(' -> State.Next Enable
    | 'n' -> State.Next (Disable '\'')
    | _ -> State.Reset
  ;;

  let handle_enable = function
    | ')' -> State.Enable
    | _ -> State.Reset
  ;;

  let handle_disable expected actual =
    let handle_correct = function
      | '\'' -> State.Next (Disable 't')
      | 't' -> State.Next (Disable '(')
      | '(' -> State.Next (Disable ')')
      | ')' -> State.Disable
      | _ -> State.Reset
    in
    if expected = actual then handle_correct actual else State.Reset
  ;;

  let initial = Begin

  let parse state =
    match State.expected state with
    | Begin -> handle_begin (State.is_toggleable state)
    | Mul expected -> handle_mul expected
    | Left_number -> handle_left
    | Right_number -> handle_right
    | Toggle -> handle_toggle
    | Enable_of_disable -> handle_enable_or_disable
    | Enable -> handle_enable
    | Disable expected -> handle_disable expected
  ;;
end

let handle state actual =
  let action = Parser.parse state actual in
  State.process action actual state
;;

let calculate is_toggleable chars =
  let initial = State.create is_toggleable Parser.initial in
  let state = Seq.fold_left handle initial chars in
  State.sum state
;;

let parse_and_calculate is_toggleable input =
  input
  |> Reader.lines_of
  |> Seq.map String.to_seq
  |> Seq.concat
  |> calculate is_toggleable
  |> string_of_int
;;

let part_one = parse_and_calculate false

let part_two = parse_and_calculate true
